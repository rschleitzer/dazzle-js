/**
 * Scheme Parser - Port from OpenJade style/SchemeParser.cxx
 *
 * Parses R4RS Scheme syntax into AST (S-expressions represented as ELObj).
 * This is a simplified port focusing on core Scheme syntax.
 *
 * Port from: SchemeParser.cxx (2,300 lines) - starting with tokenizer and datum parser
 */
import { type ELObj } from './elobj.js';
/**
 * Scheme Parser
 * Port from: SchemeParser class
 */
export declare class Parser {
    private input;
    private pos;
    private line;
    private column;
    private currentToken?;
    constructor(input: string);
    /**
     * Parse the input into a list of datums
     */
    parse(): ELObj[];
    /**
     * Parse a single datum
     * Port from: SchemeParser::parseDatum
     */
    private parseDatum;
    /**
     * Parse hash expressions: #t, #f, #\c, #(...)
     */
    private parseHashExpr;
    /**
     * Parse character literal
     * Port from: SchemeParser::parseDatum handling #\
     */
    private parseChar;
    /**
     * Convert character name to character
     * Port from: Interpreter::convertCharName
     */
    private convertCharName;
    /**
     * Parse string literal
     * Port from: SchemeParser::scanString
     */
    private parseString;
    /**
     * Parse number
     * Port from: SchemeParser::handleNumber and Interpreter::convertNumber
     */
    private parseNumber;
    /**
     * Parse number with specific radix (#b, #o, #d, #x)
     */
    private parseNumberWithRadix;
    /**
     * Parse list
     * Port from: SchemeParser::parseDatum handling tokenOpenParen
     */
    private parseList;
    /**
     * Parse vector
     * Port from: SchemeParser::parseDatum handling tokenVector
     */
    private parseVector;
    /**
     * Parse identifier/symbol
     * Port from: SchemeParser::handleIdentifier
     */
    private parseIdentifier;
    private skipWhitespaceAndComments;
    private isWhitespace;
    private isDigit;
    private isHexDigit;
    private isAlpha;
    private isIdentifierStart;
    private isIdentifierChar;
    private isAtEnd;
    private peek;
    private peekAhead;
    private advance;
    private expect;
    private currentLocation;
    private error;
}
/**
 * Parse Scheme source code
 */
export declare function parse(source: string): ELObj[];
/**
 * Parse a single Scheme expression
 */
export declare function parseOne(source: string): ELObj;
//# sourceMappingURL=parser.d.ts.map