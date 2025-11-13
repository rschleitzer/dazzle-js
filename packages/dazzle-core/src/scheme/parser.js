/**
 * Scheme Parser - Port from OpenJade style/SchemeParser.cxx
 *
 * Parses R4RS Scheme syntax into AST (S-expressions represented as ELObj).
 * This is a simplified port focusing on core Scheme syntax.
 *
 * Port from: SchemeParser.cxx (2,300 lines) - starting with tokenizer and datum parser
 */
import { makeNumber, makeSymbol, makeString, makeChar, makePair, makeVector, theNilObj, theTrueObj, theFalseObj, } from './elobj.js';
/**
 * Token types
 * Port from: SchemeParser.cxx enum Token
 */
var TokenType;
(function (TokenType) {
    TokenType["EOF"] = "EOF";
    TokenType["OpenParen"] = "(";
    TokenType["CloseParen"] = ")";
    TokenType["Quote"] = "'";
    TokenType["Quasiquote"] = "`";
    TokenType["Unquote"] = ",";
    TokenType["UnquoteSplicing"] = ",@";
    TokenType["Period"] = ".";
    TokenType["VectorStart"] = "#(";
    TokenType["True"] = "#t";
    TokenType["False"] = "#f";
    TokenType["Number"] = "number";
    TokenType["String"] = "string";
    TokenType["Char"] = "char";
    TokenType["Identifier"] = "identifier";
})(TokenType || (TokenType = {}));
/**
 * Scheme Parser
 * Port from: SchemeParser class
 */
export class Parser {
    input;
    pos = 0;
    line = 1;
    column = 1;
    currentToken;
    constructor(input) {
        this.input = input;
    }
    /**
     * Parse the input into a list of datums
     */
    parse() {
        const datums = [];
        while (!this.isAtEnd()) {
            this.skipWhitespaceAndComments();
            if (this.isAtEnd())
                break;
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
    parseDatum() {
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
    parseHashExpr() {
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
    parseChar() {
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
    convertCharName(name) {
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
    parseString() {
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
                    case 'n':
                        str += '\n';
                        break;
                    case 't':
                        str += '\t';
                        break;
                    case 'r':
                        str += '\r';
                        break;
                    case '\\':
                        str += '\\';
                        break;
                    case '"':
                        str += '"';
                        break;
                    default:
                        str += c;
                        break;
                }
            }
            else {
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
    parseNumber() {
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
    parseNumberWithRadix(radix) {
        const radixMap = {
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
    parseList() {
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
                tail.asPair().cdr = cdr;
                this.skipWhitespaceAndComments();
                this.expect(')');
                break;
            }
            const datum = this.parseDatum();
            if (!datum) {
                throw this.error('Expected datum in list');
            }
            const newPair = makePair(datum, theNilObj);
            tail.asPair().cdr = newPair;
            tail = newPair;
        }
        return head;
    }
    /**
     * Parse vector
     * Port from: SchemeParser::parseDatum handling tokenVector
     */
    parseVector() {
        // Already consumed #(
        const elements = [];
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
     * Parse identifier/symbol
     * Port from: SchemeParser::handleIdentifier
     */
    parseIdentifier() {
        let id = '';
        while (!this.isAtEnd() && this.isIdentifierChar(this.peek())) {
            id += this.advance();
        }
        if (id.length === 0) {
            throw this.error('Expected identifier');
        }
        return makeSymbol(id);
    }
    // ============ Tokenizer helpers ============
    skipWhitespaceAndComments() {
        while (!this.isAtEnd()) {
            const c = this.peek();
            if (c === ' ' || c === '\t' || c === '\r' || c === '\n' || c === '\f') {
                this.advance();
            }
            else if (c === ';') {
                // Skip comment until end of line
                while (!this.isAtEnd() && this.peek() !== '\n') {
                    this.advance();
                }
            }
            else {
                break;
            }
        }
    }
    isWhitespace(c) {
        return c === ' ' || c === '\t' || c === '\r' || c === '\n' || c === '\f';
    }
    isDigit(c) {
        return c >= '0' && c <= '9';
    }
    isHexDigit(c) {
        return this.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }
    isAlpha(c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }
    isIdentifierStart(c) {
        return this.isAlpha(c) || '!$%&*/:<=>?^_~+-'.includes(c);
    }
    isIdentifierChar(c) {
        return this.isIdentifierStart(c) || this.isDigit(c) || c === '.' || c === '@';
    }
    isAtEnd() {
        return this.pos >= this.input.length;
    }
    peek() {
        if (this.isAtEnd())
            return '\0';
        return this.input[this.pos];
    }
    peekAhead(n) {
        if (this.pos + n >= this.input.length)
            return '\0';
        return this.input[this.pos + n];
    }
    advance() {
        const c = this.input[this.pos++];
        if (c === '\n') {
            this.line++;
            this.column = 1;
        }
        else {
            this.column++;
        }
        return c;
    }
    expect(expected) {
        const c = this.peek();
        if (c !== expected) {
            throw this.error(`Expected '${expected}', got '${c}'`);
        }
        this.advance();
    }
    currentLocation() {
        return { line: this.line, column: this.column };
    }
    error(message) {
        const loc = this.currentLocation();
        return new Error(`Parse error at ${loc.line}:${loc.column}: ${message}`);
    }
}
/**
 * Parse Scheme source code
 */
export function parse(source) {
    const parser = new Parser(source);
    return parser.parse();
}
/**
 * Parse a single Scheme expression
 */
export function parseOne(source) {
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
//# sourceMappingURL=parser.js.map