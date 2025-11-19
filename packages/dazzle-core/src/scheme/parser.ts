/**
 * Scheme Parser - Port from OpenJade style/SchemeParser.cxx
 *
 * Parses R4RS Scheme syntax into AST (S-expressions represented as ELObj).
 * This is a simplified port focusing on core Scheme syntax.
 *
 * Port from: SchemeParser.cxx (2,300 lines) - starting with tokenizer and datum parser
 */

import {
  type ELObj,
  makeNumber,
  makeBoolean,
  makeSymbol,
  makeKeyword,
  makeString,
  makeChar,
  makePair,
  makeVector,
  makeUnresolvedQuantity,
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj.js';

import type { SourceMapEntry } from '../dsssl/template-loader.js';

/**
 * Token types
 * Port from: SchemeParser.cxx enum Token
 */
enum TokenType {
  EOF = 'EOF',
  OpenParen = '(',
  CloseParen = ')',
  Quote = "'",
  Quasiquote = '`',
  Unquote = ',',
  UnquoteSplicing = ',@',
  Period = '.',
  VectorStart = '#(',
  True = '#t',
  False = '#f',
  Number = 'number',
  String = 'string',
  Char = 'char',
  Identifier = 'identifier',
}

/**
 * Token
 */
interface Token {
  type: TokenType;
  value?: string | number | boolean;
  location: Location;
}

/**
 * Location in source
 */
interface Location {
  file: string;
  line: number;
  column: number;
}

/**
 * Scheme Parser
 * Port from: SchemeParser class
 */
export class Parser {
  private input: string;
  private pos: number = 0;
  private line: number = 1;
  private column: number = 1;
  private currentToken?: Token;
  private file: string;
  private sourceMap: SourceMapEntry[] | null;

  constructor(input: string, file: string = '<unknown>', sourceMap: SourceMapEntry[] | null = null) {
    this.input = input;
    this.file = file;
    this.sourceMap = sourceMap;
  }

  /**
   * Parse the input into a list of datums
   */
  parse(): ELObj[] {
    const datums: ELObj[] = [];

    while (!this.isAtEnd()) {
      this.skipWhitespaceAndComments();
      if (this.isAtEnd()) break;

      const datum = this.parseDatum();
      if (datum) {
        datums.push(datum);
      }
    }

    return datums;
  }

  /**
   * Parse a single datum
   * Port from: SchemeParser::parseDatum
   */
  private parseDatum(): ELObj | null {
    this.skipWhitespaceAndComments();

    if (this.isAtEnd()) {
      return null;
    }

    const c = this.peek();
    const loc = this.currentLocation();

    // Self-evaluating
    if (c === '#') {
      return this.parseHashExpr();
    }

    if (c === '"') {
      return this.parseString();
    }

    if (this.isDigit(c) || (c === '-' && this.isDigit(this.peekAhead(1)))) {
      return this.parseNumber();
    }

    // Quote abbreviations
    if (c === "'") {
      this.advance();
      const quoted = this.parseDatum();
      if (!quoted) {
        throw this.error('Expected expression after quote');
      }
      const pair = makePair(makeSymbol('quote'), makePair(quoted, theNilObj));
      pair.location = loc;
      return pair;
    }

    if (c === '`') {
      this.advance();
      const quoted = this.parseDatum();
      if (!quoted) {
        throw this.error('Expected expression after quasiquote');
      }
      const pair = makePair(makeSymbol('quasiquote'), makePair(quoted, theNilObj));
      pair.location = loc;
      return pair;
    }

    if (c === ',') {
      this.advance();
      const symbol = this.peek() === '@' ? (this.advance(), 'unquote-splicing') : 'unquote';
      const quoted = this.parseDatum();
      if (!quoted) {
        throw this.error(`Expected expression after ${symbol}`);
      }
      const pair = makePair(makeSymbol(symbol), makePair(quoted, theNilObj));
      pair.location = loc;
      return pair;
    }

    // Lists
    if (c === '(') {
      return this.parseList();
    }

    // Identifiers/Symbols
    if (this.isIdentifierStart(c)) {
      return this.parseIdentifier();
    }

    throw this.error(`Unexpected character: ${c}`);
  }

  /**
   * Parse hash expressions: #t, #f, #\c, #(...)
   */
  private parseHashExpr(): ELObj {
    this.expect('#');
    const c = this.peek();

    if (c === 't') {
      this.advance();
      return theTrueObj;
    }

    if (c === 'f') {
      this.advance();
      return theFalseObj;
    }

    if (c === '\\') {
      this.advance();
      return this.parseChar();
    }

    if (c === '(') {
      this.advance();
      return this.parseVector();
    }

    // Number radix: #b, #o, #d, #x
    if (c === 'b' || c === 'o' || c === 'd' || c === 'x') {
      this.advance();
      return this.parseNumberWithRadix(c);
    }

    // #!rest and other #! keywords
    // Port from: OpenJade supports #!rest for variadic arguments
    if (c === '!') {
      this.advance();
      // Read the keyword name
      const start = this.pos;
      while (this.pos < this.input.length && this.isIdentifierChar(this.peek())) {
        this.advance();
      }
      const keyword = this.input.substring(start, this.pos);
      // Return as a special symbol with #! prefix
      return makeSymbol('#!' + keyword);
    }

    throw this.error(`Invalid hash expression: #${c}`);
  }

  /**
   * Parse character literal
   * Port from: SchemeParser::parseDatum handling #\
   *
   * Supports DSSSL SGML entity references: #\&NAME
   * where NAME is an SGML function character name (RE, RS, SPACE, TAB)
   */
  private parseChar(): ELObj {
    const start = this.pos;
    const ch = this.peek();

    // DSSSL SGML entity reference: #\&name
    // Port from: OpenJade SGML function character support
    if (ch === '&') {
      this.advance(); // skip '&'
      const nameStart = this.pos;

      // Read entity name (alphanumeric, hyphen, and # for numeric refs)
      while (this.pos < this.input.length &&
             (this.isAlpha(this.peek()) || this.peek() === '-' || this.peek() === '#')) {
        this.advance();
      }

      const entityName = this.input.substring(nameStart, this.pos);
      const entityChar = this.convertSgmlEntityName(entityName);
      return makeChar(entityChar);
    }

    // Single character (non-alphabetic)
    if (!this.isAlpha(ch)) {
      this.advance();
      return makeChar(ch);
    }

    // Named character (newline, space, tab, etc.)
    while (this.isAlpha(this.peek())) {
      this.advance();
    }

    const name = this.input.substring(start, this.pos);
    const convertedChar = this.convertCharName(name);
    return makeChar(convertedChar);
  }

  /**
   * Convert character name to character
   * Port from: Interpreter::convertCharName
   */
  private convertCharName(name: string): string {
    const lower = name.toLowerCase();

    switch (lower) {
      case 'space': return ' ';
      case 'newline': return '\n';
      case 'tab': return '\t';
      case 'return': return '\r';
      case 'linefeed': return '\n';
      case 'page': return '\f';
      case 'null': return '\0';
      default:
        if (name.length === 1) {
          return name;
        }
        throw this.error(`Unknown character name: ${name}`);
    }
  }

  /**
   * Convert SGML entity name to character
   * Port from: OpenJade SGML function character mapping
   *
   * SGML defines standard function characters (ISO 8879):
   * - #RE (Record End): Carriage return
   * - #RS (Record Start): Line feed
   * - #SPACE: Space character
   * - #TAB: Horizontal tab
   */
  private convertSgmlEntityName(name: string): string {
    const upper = name.toUpperCase();

    switch (upper) {
      case '#RE': return '\r';        // Record End (0x000D)
      case '#RS': return '\n';        // Record Start (0x000A)
      case '#SPACE': return ' ';      // Space (0x0020)
      case '#TAB': return '\t';       // Tab (0x0009)
      // Also support without # prefix for compatibility
      case 'RE': return '\r';
      case 'RS': return '\n';
      case 'SPACE': return ' ';
      case 'TAB': return '\t';
      default:
        throw this.error(`Unknown SGML entity name: &${name}`);
    }
  }

  /**
   * Parse string literal
   * Port from: SchemeParser::scanString
   */
  private parseString(): ELObj {
    this.expect('"');
    let str = '';

    while (!this.isAtEnd() && this.peek() !== '"') {
      if (this.peek() === '\\') {
        this.advance();
        if (this.isAtEnd()) {
          throw this.error('Unexpected end of string');
        }
        const c = this.advance();
        switch (c) {
          case 'n': str += '\n'; break;
          case 't': str += '\t'; break;
          case 'r': str += '\r'; break;
          case '\\': str += '\\'; break;
          case '"': str += '"'; break;
          default: str += c; break;
        }
      } else {
        str += this.advance();
      }
    }

    this.expect('"');
    return makeString(str);
  }

  /**
   * Parse number
   * Port from: SchemeParser::handleNumber and Interpreter::convertNumber
   */
  private parseNumber(): ELObj {
    let numStr = '';
    const negative = this.peek() === '-';

    if (negative) {
      numStr += this.advance();
    }

    // Integer part
    while (this.isDigit(this.peek())) {
      numStr += this.advance();
    }

    // Fractional part
    if (this.peek() === '.') {
      numStr += this.advance();
      while (this.isDigit(this.peek())) {
        numStr += this.advance();
      }
    }

    // Exponent
    if (this.peek() === 'e' || this.peek() === 'E') {
      numStr += this.advance();
      if (this.peek() === '+' || this.peek() === '-') {
        numStr += this.advance();
      }
      while (this.isDigit(this.peek())) {
        numStr += this.advance();
      }
    }

    const value = parseFloat(numStr);
    if (isNaN(value)) {
      throw this.error(`Invalid number: ${numStr}`);
    }

    // Port from: OpenJade Interpreter::convertNumber and scanUnit
    // Check for unit suffix (e.g., 10cm, 12pt, 1.5in)
    let unitName = '';
    while (!this.isAtEnd() && this.isIdentifierChar(this.peek()) && !this.isDigit(this.peek())) {
      unitName += this.advance();
    }

    if (unitName) {
      // Parse optional unit exponent (e.g., cm2, m-1)
      let unitExp = 1;
      if (!this.isAtEnd() && (this.peek() === '-' || this.peek() === '+' || this.isDigit(this.peek()))) {
        let expStr = '';
        const negativeExp = this.peek() === '-';
        if (this.peek() === '-' || this.peek() === '+') {
          expStr += this.advance();
        }
        while (this.isDigit(this.peek())) {
          expStr += this.advance();
        }
        if (expStr) {
          unitExp = parseInt(expStr, 10);
          if (isNaN(unitExp)) {
            unitExp = 1;
          }
        }
      }

      // Return unresolved quantity - will be resolved later
      return makeUnresolvedQuantity(value, unitName, unitExp);
    }

    // Plain number without unit
    const exact = Number.isInteger(value);
    return makeNumber(value, exact);
  }

  /**
   * Parse number with specific radix (#b, #o, #d, #x)
   */
  private parseNumberWithRadix(radix: string): ELObj {
    const radixMap: Record<string, number> = {
      'b': 2,
      'o': 8,
      'd': 10,
      'x': 16,
    };

    let numStr = '';
    const base = radixMap[radix];

    while (!this.isAtEnd() && this.isHexDigit(this.peek())) {
      numStr += this.advance();
    }

    if (numStr.length === 0) {
      throw this.error(`Expected digits after #${radix}`);
    }

    const value = parseInt(numStr, base);
    return makeNumber(value, true); // Always exact for radix notation
  }

  /**
   * Parse list
   * Port from: SchemeParser::parseDatum handling tokenOpenParen
   */
  private parseList(): ELObj {
    const listLoc = this.currentLocation();
    this.expect('(');
    this.skipWhitespaceAndComments();

    // Empty list
    if (this.peek() === ')') {
      this.advance();
      return theNilObj;
    }

    // Build list
    const first = this.parseDatum();
    if (!first) {
      throw this.error('Expected datum in list');
    }

    let head = makePair(first, theNilObj);
    head.location = listLoc; // Attach location to first pair (the list itself)
    let tail = head;

    while (true) {
      this.skipWhitespaceAndComments();

      if (this.peek() === ')') {
        this.advance();
        // Proper list - already terminated with nil
        break;
      }

      if (this.peek() === '.') {
        // Improper list: (a . b)
        this.advance();
        const cdr = this.parseDatum();
        if (!cdr) {
          throw this.error('Expected datum after dot');
        }
        tail.asPair()!.cdr = cdr;
        this.skipWhitespaceAndComments();
        this.expect(')');
        break;
      }

      const datum = this.parseDatum();
      if (!datum) {
        throw this.error('Expected datum in list');
      }

      const newPair = makePair(datum, theNilObj);
      newPair.location = this.currentLocation();
      tail.asPair()!.cdr = newPair;
      tail = newPair;
    }

    return head;
  }

  /**
   * Parse vector
   * Port from: SchemeParser::parseDatum handling tokenVector
   */
  private parseVector(): ELObj {
    // Already consumed #(
    const elements: ELObj[] = [];

    while (true) {
      this.skipWhitespaceAndComments();

      if (this.peek() === ')') {
        this.advance();
        break;
      }

      const datum = this.parseDatum();
      if (!datum) {
        throw this.error('Expected datum in vector');
      }
      elements.push(datum);
    }

    return makeVector(elements);
  }

  /**
   * Parse identifier/symbol/keyword
   * Port from: SchemeParser::handleIdentifier
   *
   * DSSSL keywords end with ':' (e.g., system-id:, data:)
   */
  private parseIdentifier(): ELObj {
    let id = '';

    while (!this.isAtEnd() && this.isIdentifierChar(this.peek())) {
      id += this.advance();
    }

    if (id.length === 0) {
      throw this.error('Expected identifier');
    }

    // DSSSL keyword: trailing colon (e.g., system-id:, data:)
    if (id.endsWith(':')) {
      return makeKeyword(id.slice(0, -1)); // Remove trailing colon
    }

    return makeSymbol(id);
  }

  // ============ Tokenizer helpers ============

  private skipWhitespaceAndComments(): void {
    while (!this.isAtEnd()) {
      const c = this.peek();

      if (c === ' ' || c === '\t' || c === '\r' || c === '\n' || c === '\f') {
        this.advance();
      } else if (c === ';') {
        // Skip comment until end of line
        while (!this.isAtEnd() && this.peek() !== '\n') {
          this.advance();
        }
      } else {
        break;
      }
    }
  }

  private isWhitespace(c: string): boolean {
    return c === ' ' || c === '\t' || c === '\r' || c === '\n' || c === '\f';
  }

  private isDigit(c: string): boolean {
    return c >= '0' && c <= '9';
  }

  private isHexDigit(c: string): boolean {
    return this.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
  }

  private isAlpha(c: string): boolean {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }

  private isIdentifierStart(c: string): boolean {
    return this.isAlpha(c) || '!$%&*/:<=>?^_~+-'.includes(c);
  }

  private isIdentifierChar(c: string): boolean {
    return this.isIdentifierStart(c) || this.isDigit(c) || c === '.' || c === '@';
  }

  private isAtEnd(): boolean {
    return this.pos >= this.input.length;
  }

  private peek(): string {
    if (this.isAtEnd()) return '\0';
    return this.input[this.pos];
  }

  private peekAhead(n: number): string {
    if (this.pos + n >= this.input.length) return '\0';
    return this.input[this.pos + n];
  }

  private advance(): string {
    const c = this.input[this.pos++];
    if (c === '\n') {
      this.line++;
      this.column = 1;
    } else {
      this.column++;
    }
    return c;
  }

  private expect(expected: string): void {
    const c = this.peek();
    if (c !== expected) {
      throw this.error(`Expected '${expected}', got '${c}'`);
    }
    this.advance();
  }

  private currentLocation(): Location {
    // Look up actual source file from source map
    if (this.sourceMap) {
      for (const entry of this.sourceMap) {
        if (this.line >= entry.startLine && this.line <= entry.endLine) {
          // Found the source map entry for this line
          const offsetInEntry = this.line - entry.startLine;
          return {
            file: entry.sourceFile,
            line: entry.sourceLine + offsetInEntry + 1, // Convert to 1-based
            column: this.column,
          };
        }
      }
    }

    // Fallback: no source map or line not found
    return { file: this.file, line: this.line, column: this.column };
  }

  private error(message: string): Error {
    const loc = this.currentLocation();
    return new Error(`Parse error in ${loc.file} at ${loc.line}:${loc.column}: ${message}`);
  }
}

/**
 * Parse Scheme source code
 */
export function parse(source: string, file: string = '<unknown>', sourceMap: SourceMapEntry[] | null = null): ELObj[] {
  const parser = new Parser(source, file, sourceMap);
  return parser.parse();
}

/**
 * Parse a single Scheme expression
 */
export function parseOne(source: string): ELObj {
  const parser = new Parser(source);
  const datums = parser.parse();
  if (datums.length === 0) {
    throw new Error('No expression to parse');
  }
  if (datums.length > 1) {
    throw new Error('Multiple expressions found, expected one');
  }
  return datums[0];
}
