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
  theNilObj,
  theTrueObj,
  theFalseObj,
} from './elobj.js';

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

  constructor(input: string) {
    this.input = input;
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
    const currentToken = this.currentLocation();

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
      return makePair(makeSymbol('quote'), makePair(quoted, theNilObj));
    }

    if (c === '`') {
      this.advance();
      const quoted = this.parseDatum();
      if (!quoted) {
        throw this.error('Expected expression after quasiquote');
      }
      return makePair(makeSymbol('quasiquote'), makePair(quoted, theNilObj));
    }

    if (c === ',') {
      this.advance();
      const symbol = this.peek() === '@' ? (this.advance(), 'unquote-splicing') : 'unquote';
      const quoted = this.parseDatum();
      if (!quoted) {
        throw this.error(`Expected expression after ${symbol}`);
      }
      return makePair(makeSymbol(symbol), makePair(quoted, theNilObj));
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

    throw this.error(`Invalid hash expression: #${c}`);
  }

  /**
   * Parse character literal
   * Port from: SchemeParser::parseDatum handling #\
   */
  private parseChar(): ELObj {
    const start = this.pos;

    // Single character
    if (!this.isAlpha(this.peek())) {
      const ch = this.advance();
      return makeChar(ch);
    }

    // Named character (newline, space, tab, etc.)
    while (this.isAlpha(this.peek())) {
      this.advance();
    }

    const name = this.input.substring(start, this.pos);
    const ch = this.convertCharName(name);
    return makeChar(ch);
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

    // Check if exact (integer)
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
    return { line: this.line, column: this.column };
  }

  private error(message: string): Error {
    const loc = this.currentLocation();
    return new Error(`Parse error at ${loc.line}:${loc.column}: ${message}`);
  }
}

/**
 * Parse Scheme source code
 */
export function parse(source: string): ELObj[] {
  const parser = new Parser(source);
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
