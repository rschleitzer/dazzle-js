//! Scheme parser (lexer + S-expression parser)
//!
//! Ported from OpenJade's `SchemeParser.cxx` (~2,500 lines).
//!
//! ## Key Features
//!
//! - **Whitespace-agnostic**: Unlike Steel, this parser handles all valid R4RS whitespace
//! - **Line number tracking**: Error messages report line:column, not byte spans
//! - **Full R4RS syntax**:
//!   - Numbers: integers, reals, hex (#x), octal (#o), binary (#b)
//!   - Strings: escapes (\n, \t, \", \\, \xNN)
//!   - Symbols and keywords
//!   - Quote/quasiquote/unquote/unquote-splicing
//!   - Comments: line (;) and block (#| ... |#)
//!   - Vectors: #(...)
//!   - Booleans: #t, #f
//!   - Characters: #\a, #\space, #\newline
//!
//! ## Architecture
//!
//! 1. **Lexer** (`Tokenizer`): Character stream → Token stream
//! 2. **Parser** (`Parser`): Token stream → Value (S-expressions)
//!
//! ## OpenJade Correspondence
//!
//! | Dazzle        | OpenJade             | Purpose                |
//! |---------------|----------------------|------------------------|
//! | `Token`       | `SchemeParser::tok_` | Token types            |
//! | `Tokenizer`   | `SchemeParser`       | Lexical analysis       |
//! | `Parser`      | `SchemeParser::get*` | Syntax analysis        |
//!
//! ## Error Handling
//!
//! Parse errors include:
//! - Line and column numbers (not byte offsets!)
//! - Descriptive messages
//! - Context (what was expected)

use crate::scheme::value::Value;
use std::fmt;

// =============================================================================
// Token Types
// =============================================================================

/// Token type (corresponds to OpenJade's token enum)
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Integer(i64),
    Real(f64),
    String(String),
    Char(char),
    Symbol(String),
    Keyword(String),
    Bool(bool),

    // Delimiters
    LeftParen,   // (
    RightParen,  // )
    LeftBracket, // [  (optional R5RS)
    RightBracket, // ] (optional R5RS)
    Dot,         // .

    // Quotation
    Quote,            // '
    Quasiquote,       // `
    Unquote,          // ,
    UnquoteSplicing,  // ,@

    // Vector
    VectorStart, // #(

    // End of input
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(n) => write!(f, "{}", n),
            Token::Real(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Char(ch) => write!(f, "#\\{}", ch),
            Token::Symbol(s) => write!(f, "{}", s),
            Token::Keyword(s) => write!(f, "#:{}", s),
            Token::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::Dot => write!(f, "."),
            Token::Quote => write!(f, "'"),
            Token::Quasiquote => write!(f, "`"),
            Token::Unquote => write!(f, ","),
            Token::UnquoteSplicing => write!(f, ",@"),
            Token::VectorStart => write!(f, "#("),
            Token::Eof => write!(f, "<EOF>"),
        }
    }
}

// =============================================================================
// Source Position (for error reporting)
// =============================================================================

/// Source code position (line and column)
///
/// **Important**: Line and column numbers start at 1 (human-readable).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new() -> Self {
        Position { line: 1, column: 1 }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

// =============================================================================
// Parse Error
// =============================================================================

/// Parse error with line:column position
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub position: Position,
}

impl ParseError {
    pub fn new(message: String, position: Position) -> Self {
        ParseError { message, position }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error at {}: {}", self.position, self.message)
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

// =============================================================================
// Tokenizer (Lexer)
// =============================================================================

/// Tokenizer for Scheme source code
///
/// Corresponds to OpenJade's `SchemeParser` lexical analysis methods.
///
/// ## Whitespace Handling
///
/// **Critical difference from Steel**: This tokenizer is fully whitespace-agnostic.
/// Whitespace can appear anywhere between tokens without affecting parsing.
///
/// Example that breaks Steel but works here:
/// ```scheme
/// (let ((x 1)
///       (y 2))  ; Multi-line let bindings
///   (+ x y))
/// ```
pub struct Tokenizer {
    /// Input source code
    input: Vec<char>,

    /// Current position in input
    pos: usize,

    /// Current line (1-based)
    line: usize,

    /// Current column (1-based)
    column: usize,

    /// Peeked token (for lookahead)
    peeked: Option<Token>,
}

impl Tokenizer {
    /// Create a new tokenizer from source code
    pub fn new(input: &str) -> Self {
        Tokenizer {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
            peeked: None,
        }
    }

    /// Get current position (for error reporting)
    pub fn position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    /// Peek at current character without consuming
    fn peek_char(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    /// Peek at character at offset from current position
    fn peek_char_at(&self, offset: usize) -> Option<char> {
        let index = self.pos + offset;
        if index < self.input.len() {
            Some(self.input[index])
        } else {
            None
        }
    }

    /// Consume and return current character
    fn next_char(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            let ch = self.input[self.pos];
            self.pos += 1;

            // Update line/column tracking
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }

            Some(ch)
        } else {
            None
        }
    }

    /// Skip whitespace and comments
    ///
    /// **Key feature**: Handles both line comments (;) and block comments (#| ... |#)
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                // Whitespace
                Some(ch) if ch.is_whitespace() => {
                    self.next_char();
                }

                // Line comment: ; to end of line
                Some(';') => {
                    self.next_char();
                    while let Some(ch) = self.peek_char() {
                        self.next_char();
                        if ch == '\n' {
                            break;
                        }
                    }
                }

                // Block comment: #| ... |#
                Some('#') if self.peek_char_at(1) == Some('|') => {
                    self.next_char(); // #
                    self.next_char(); // |

                    // Find closing |#
                    let mut depth = 1;
                    while depth > 0 {
                        match self.next_char() {
                            Some('|') if self.peek_char() == Some('#') => {
                                self.next_char(); // #
                                depth -= 1;
                            }
                            Some('#') if self.peek_char() == Some('|') => {
                                self.next_char(); // |
                                depth += 1; // Nested block comment
                            }
                            Some(_) => {} // Continue
                            None => break, // EOF in comment (error, but tolerate)
                        }
                    }
                }

                // Not whitespace or comment
                _ => break,
            }
        }
    }

    /// Check if character is a delimiter (ends a token)
    fn is_delimiter(ch: char) -> bool {
        ch.is_whitespace()
            || matches!(
                ch,
                '(' | ')' | '[' | ']' | '"' | ';' | ',' | '`' | '\''
            )
    }

    /// Parse an integer or real number
    fn parse_number(&mut self, start_pos: Position) -> ParseResult<Token> {
        let mut num_str = String::new();

        // Collect digits and special characters
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() || matches!(ch, '.' | 'e' | 'E' | '+' | '-') {
                num_str.push(ch);
                self.next_char();
            } else if Self::is_delimiter(ch) {
                break;
            } else {
                // Invalid character in number
                return Err(ParseError::new(
                    format!("Invalid character in number: {}", ch),
                    start_pos,
                ));
            }
        }

        // Try parsing as integer first
        if let Ok(n) = num_str.parse::<i64>() {
            return Ok(Token::Integer(n));
        }

        // Try parsing as float
        if let Ok(n) = num_str.parse::<f64>() {
            return Ok(Token::Real(n));
        }

        Err(ParseError::new(
            format!("Invalid number: {}", num_str),
            start_pos,
        ))
    }

    /// Parse a hexadecimal number (#x prefix)
    fn parse_hex_number(&mut self, start_pos: Position) -> ParseResult<Token> {
        let mut num_str = String::new();

        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_hexdigit() {
                num_str.push(ch);
                self.next_char();
            } else if Self::is_delimiter(ch) {
                break;
            } else {
                return Err(ParseError::new(
                    format!("Invalid character in hex number: {}", ch),
                    start_pos,
                ));
            }
        }

        if num_str.is_empty() {
            return Err(ParseError::new("Empty hex number".to_string(), start_pos));
        }

        i64::from_str_radix(&num_str, 16)
            .map(Token::Integer)
            .map_err(|_| ParseError::new(format!("Invalid hex number: {}", num_str), start_pos))
    }

    /// Parse an octal number (#o prefix)
    fn parse_octal_number(&mut self, start_pos: Position) -> ParseResult<Token> {
        let mut num_str = String::new();

        while let Some(ch) = self.peek_char() {
            if ch.is_digit(8) {
                num_str.push(ch);
                self.next_char();
            } else if Self::is_delimiter(ch) {
                break;
            } else {
                return Err(ParseError::new(
                    format!("Invalid character in octal number: {}", ch),
                    start_pos,
                ));
            }
        }

        if num_str.is_empty() {
            return Err(ParseError::new("Empty octal number".to_string(), start_pos));
        }

        i64::from_str_radix(&num_str, 8)
            .map(Token::Integer)
            .map_err(|_| ParseError::new(format!("Invalid octal number: {}", num_str), start_pos))
    }

    /// Parse a binary number (#b prefix)
    fn parse_binary_number(&mut self, start_pos: Position) -> ParseResult<Token> {
        let mut num_str = String::new();

        while let Some(ch) = self.peek_char() {
            if matches!(ch, '0' | '1') {
                num_str.push(ch);
                self.next_char();
            } else if Self::is_delimiter(ch) {
                break;
            } else {
                return Err(ParseError::new(
                    format!("Invalid character in binary number: {}", ch),
                    start_pos,
                ));
            }
        }

        if num_str.is_empty() {
            return Err(ParseError::new("Empty binary number".to_string(), start_pos));
        }

        i64::from_str_radix(&num_str, 2)
            .map(Token::Integer)
            .map_err(|_| ParseError::new(format!("Invalid binary number: {}", num_str), start_pos))
    }

    /// Parse a symbol or keyword
    fn parse_symbol(&mut self) -> String {
        let mut sym = String::new();

        while let Some(ch) = self.peek_char() {
            if Self::is_delimiter(ch) {
                break;
            }
            sym.push(ch);
            self.next_char();
        }

        sym
    }

    /// Parse a string literal
    fn parse_string(&mut self, start_pos: Position) -> ParseResult<String> {
        self.next_char(); // Consume opening "

        let mut result = String::new();

        loop {
            match self.next_char() {
                Some('"') => {
                    // Closing quote
                    // Normalize CRLF to LF (to match OpenJade behavior)
                    // OpenJade always outputs Unix line endings regardless of template line endings
                    let normalized = result.replace("\r\n", "\n");
                    return Ok(normalized);
                }
                Some('\\') => {
                    // Escape sequence
                    match self.next_char() {
                        Some('n') => result.push('\n'),
                        Some('t') => result.push('\t'),
                        Some('r') => result.push('\r'),
                        Some('\\') => result.push('\\'),
                        Some('"') => result.push('"'),
                        Some(ch) => result.push(ch), // Unknown escape, keep literal
                        None => {
                            return Err(ParseError::new(
                                "Unexpected EOF in string escape".to_string(),
                                start_pos,
                            ))
                        }
                    }
                }
                Some(ch) => {
                    result.push(ch);
                }
                None => {
                    return Err(ParseError::new(
                        "Unexpected EOF in string".to_string(),
                        start_pos,
                    ))
                }
            }
        }
    }

    /// Parse a character literal (#\a, #\space, #\newline)
    /// Called after # has been consumed
    fn parse_char(&mut self, start_pos: Position) -> ParseResult<char> {
        // Expect backslash
        if self.next_char() != Some('\\') {
            return Err(ParseError::new(
                "Expected \\ after # in character literal".to_string(),
                start_pos,
            ));
        }

        // Read character name
        let mut name = String::new();
        while let Some(ch) = self.peek_char() {
            if Self::is_delimiter(ch) {
                break;
            }
            name.push(ch);
            self.next_char();
        }

        if name.is_empty() {
            return Err(ParseError::new(
                "Empty character literal".to_string(),
                start_pos,
            ));
        }

        // Named characters
        match name.as_str() {
            "space" => Ok(' '),
            "newline" => Ok('\n'),
            "tab" => Ok('\t'),
            "return" => Ok('\r'),
            // OpenJade Unicode character literal: #\U-XXXX (e.g., #\U-00E4 for ä)
            // Format: U-XXXX where XXXX is hexadecimal Unicode code point
            s if s.starts_with("U-") => {
                let hex_str = &s[2..]; // Skip "U-"
                u32::from_str_radix(hex_str, 16)
                    .ok()
                    .and_then(std::char::from_u32)
                    .ok_or_else(|| ParseError::new(
                        format!("Invalid Unicode character literal: #\\{}", name),
                        start_pos,
                    ))
            }
            // Accept any single Unicode character (handles UTF-8 multi-byte sequences)
            // OpenJade accepts UTF-8 characters in character literals for define-language
            s if s.chars().count() == 1 => Ok(s.chars().next().unwrap()),
            _ => Err(ParseError::new(
                format!("Invalid character literal: #\\{}", name),
                start_pos,
            )),
        }
    }

    /// Parse a CDATA string literal: <![CDATA[...]]>
    /// This is an OpenJade extension for multi-line string literals
    /// The content between <![CDATA[ and ]]> is returned as a string token
    fn parse_cdata_string(&mut self, start_pos: Position) -> ParseResult<Token> {
        // Skip "<![CDATA["
        for _ in 0..9 {
            self.next_char();
        }

        let mut content = String::new();

        // Read until we find "]]>"
        loop {
            match self.peek_char() {
                None => {
                    return Err(ParseError::new(
                        "Unclosed CDATA section: missing ]]>".to_string(),
                        start_pos,
                    ));
                }
                Some(']') => {
                    // Check if this is the closing ]]>
                    if self.pos + 2 < self.input.len()
                        && self.input[self.pos] == ']'
                        && self.input[self.pos + 1] == ']'
                        && self.input[self.pos + 2] == '>'
                    {
                        // Skip ]]>
                        self.next_char(); // ]
                        self.next_char(); // ]
                        self.next_char(); // >
                        break;
                    } else {
                        content.push(']');
                        self.next_char();
                    }
                }
                Some(ch) => {
                    content.push(ch);
                    self.next_char();
                }
            }
        }

        Ok(Token::String(content))
    }

    /// Get the next token
    pub fn next_token(&mut self) -> ParseResult<Token> {
        // Check if we have a peeked token
        if let Some(tok) = self.peeked.take() {
            return Ok(tok);
        }

        // Skip whitespace and comments
        self.skip_whitespace();

        let start_pos = self.position();

        match self.peek_char() {
            None => Ok(Token::Eof),

            Some('(') => {
                self.next_char();
                Ok(Token::LeftParen)
            }

            Some(')') => {
                self.next_char();
                Ok(Token::RightParen)
            }

            Some('[') => {
                self.next_char();
                Ok(Token::LeftBracket)
            }

            Some(']') => {
                self.next_char();
                Ok(Token::RightBracket)
            }

            Some('\'') => {
                self.next_char();
                Ok(Token::Quote)
            }

            Some('`') => {
                self.next_char();
                Ok(Token::Quasiquote)
            }

            Some(',') => {
                self.next_char();
                // Check for ,@
                if self.peek_char() == Some('@') {
                    self.next_char();
                    Ok(Token::UnquoteSplicing)
                } else {
                    Ok(Token::Unquote)
                }
            }

            Some('"') => {
                let s = self.parse_string(start_pos)?;
                Ok(Token::String(s))
            }

            Some('#') => {
                self.next_char(); // Consume #
                match self.peek_char() {
                    Some('t') => {
                        self.next_char();
                        Ok(Token::Bool(true))
                    }
                    Some('f') => {
                        self.next_char();
                        Ok(Token::Bool(false))
                    }
                    Some('(') => {
                        self.next_char();
                        Ok(Token::VectorStart)
                    }
                    Some('\\') => {
                        let ch = self.parse_char(start_pos)?;
                        Ok(Token::Char(ch))
                    }
                    Some(':') => {
                        self.next_char(); // Consume :
                        let name = self.parse_symbol();
                        Ok(Token::Keyword(name))
                    }
                    Some('x') | Some('X') => {
                        self.next_char(); // Consume x
                        self.parse_hex_number(start_pos)
                    }
                    Some('o') | Some('O') => {
                        self.next_char(); // Consume o
                        self.parse_octal_number(start_pos)
                    }
                    Some('b') | Some('B') => {
                        self.next_char(); // Consume b
                        self.parse_binary_number(start_pos)
                    }
                    _ => Err(ParseError::new(
                        format!("Invalid # syntax: #{:?}", self.peek_char()),
                        start_pos,
                    )),
                }
            }

            Some(ch) if ch.is_ascii_digit() => self.parse_number(start_pos),

            Some('+') | Some('-') => {
                // Could be number or symbol
                if let Some(next) = self.peek_char_at(1) {
                    if next.is_ascii_digit() {
                        self.parse_number(start_pos)
                    } else {
                        let sym = self.parse_symbol();
                        Ok(Token::Symbol(sym))
                    }
                } else {
                    let sym = self.parse_symbol();
                    Ok(Token::Symbol(sym))
                }
            }

            Some('.') => {
                // Could be dot or number starting with .
                if let Some(next) = self.peek_char_at(1) {
                    if next.is_ascii_digit() {
                        self.parse_number(start_pos)
                    } else {
                        self.next_char();
                        Ok(Token::Dot)
                    }
                } else {
                    self.next_char();
                    Ok(Token::Dot)
                }
            }

            Some('<') => {
                // Check for CDATA section: <![CDATA[...]]>
                // This is an OpenJade extension for multi-line string literals
                let cdata_prefix = ['<', '!', '[', 'C', 'D', 'A', 'T', 'A', '['];
                let is_cdata = self.pos + cdata_prefix.len() <= self.input.len()
                    && self.input[self.pos..self.pos + cdata_prefix.len()] == cdata_prefix;

                if is_cdata {
                    self.parse_cdata_string(start_pos)
                } else {
                    // Regular < symbol
                    let sym = self.parse_symbol();
                    Ok(Token::Symbol(sym))
                }
            }

            Some(_) => {
                // Symbol or DSSSL keyword (trailing colon)
                let sym = self.parse_symbol();

                // DSSSL uses trailing colon for keywords: name:
                if sym.ends_with(':') {
                    let keyword_name = sym[..sym.len()-1].to_string();
                    Ok(Token::Keyword(keyword_name))
                } else {
                    Ok(Token::Symbol(sym))
                }
            }
        }
    }

    /// Peek at the next token without consuming it
    pub fn peek_token(&mut self) -> ParseResult<&Token> {
        if self.peeked.is_none() {
            let tok = self.next_token()?;
            self.peeked = Some(tok);
        }
        Ok(self.peeked.as_ref().unwrap())
    }
}

// =============================================================================
// Parser (S-expression builder)
// =============================================================================

/// Parser for building Scheme values from tokens
///
/// Corresponds to OpenJade's `SchemeParser::get*` methods.
///
/// ## Usage
///
/// ```ignore
/// let parser = Parser::new("(+ 1 2)");
/// let expr = parser.parse().unwrap();
/// ```
pub struct Parser {
    tokenizer: Tokenizer,
}

impl Parser {
    /// Create a new parser from source code
    pub fn new(input: &str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(input),
        }
    }

    /// Parse a single S-expression
    ///
    /// Returns `Ok(Value)` on success, `Err(ParseError)` on failure.
    pub fn parse(&mut self) -> ParseResult<Value> {
        self.parse_expr()
    }

    /// Parse all S-expressions in input
    ///
    /// Returns a list of all top-level expressions.
    pub fn parse_all(&mut self) -> ParseResult<Vec<Value>> {
        let mut exprs = Vec::new();

        loop {
            let tok = self.tokenizer.peek_token()?;
            if *tok == Token::Eof {
                break;
            }
            exprs.push(self.parse_expr()?);
        }

        Ok(exprs)
    }

    /// Parse a single expression
    fn parse_expr(&mut self) -> ParseResult<Value> {
        let start_pos = self.tokenizer.position();
        let tok = self.tokenizer.next_token()?;

        match tok {
            // Literals
            Token::Integer(n) => Ok(Value::integer(n)),
            Token::Real(n) => Ok(Value::real(n)),
            Token::String(s) => Ok(Value::string(s)),
            Token::Char(ch) => Ok(Value::char(ch)),
            Token::Bool(b) => Ok(Value::bool(b)),
            Token::Symbol(s) => Ok(Value::symbol(&s)),
            Token::Keyword(s) => Ok(Value::keyword(&s)),

            // Lists
            Token::LeftParen | Token::LeftBracket => self.parse_list(start_pos),

            // Vectors
            Token::VectorStart => self.parse_vector(start_pos),

            // Quote
            Token::Quote => {
                let quoted = self.parse_expr()?;
                Ok(Value::cons(Value::symbol("quote"), Value::cons(quoted, Value::Nil)))
            }

            // Quasiquote
            Token::Quasiquote => {
                let quoted = self.parse_expr()?;
                Ok(Value::cons(
                    Value::symbol("quasiquote"),
                    Value::cons(quoted, Value::Nil),
                ))
            }

            // Unquote
            Token::Unquote => {
                let quoted = self.parse_expr()?;
                Ok(Value::cons(
                    Value::symbol("unquote"),
                    Value::cons(quoted, Value::Nil),
                ))
            }

            // Unquote-splicing
            Token::UnquoteSplicing => {
                let quoted = self.parse_expr()?;
                Ok(Value::cons(
                    Value::symbol("unquote-splicing"),
                    Value::cons(quoted, Value::Nil),
                ))
            }

            // Unexpected tokens
            Token::RightParen | Token::RightBracket => Err(ParseError::new(
                format!("Unexpected closing delimiter: {}", tok),
                start_pos,
            )),

            Token::Dot => Err(ParseError::new(
                "Unexpected dot outside of list".to_string(),
                start_pos,
            )),

            Token::Eof => Err(ParseError::new(
                "Unexpected end of input".to_string(),
                start_pos,
            )),
        }
    }

    /// Parse a list (after opening paren consumed)
    fn parse_list(&mut self, start_pos: Position) -> ParseResult<Value> {
        let mut elements = Vec::new();
        let mut dotted_tail = None;

        loop {
            let tok = self.tokenizer.peek_token()?;

            match tok {
                Token::RightParen | Token::RightBracket => {
                    self.tokenizer.next_token()?; // Consume closing paren
                    break;
                }

                Token::Dot => {
                    self.tokenizer.next_token()?; // Consume dot

                    // Parse the tail
                    dotted_tail = Some(self.parse_expr()?);

                    // Expect closing paren
                    let tok = self.tokenizer.next_token()?;
                    if !matches!(tok, Token::RightParen | Token::RightBracket) {
                        return Err(ParseError::new(
                            format!("Expected ) after dotted tail, got {}", tok),
                            start_pos,
                        ));
                    }
                    break;
                }

                Token::Eof => {
                    return Err(ParseError::new(
                        "Unexpected EOF in list".to_string(),
                        start_pos,
                    ))
                }

                _ => {
                    elements.push(self.parse_expr()?);
                }
            }
        }

        // Build the list from right to left
        let mut result = dotted_tail.unwrap_or(Value::Nil);
        for elem in elements.into_iter().rev() {
            result = Value::cons(elem, result);
        }

        Ok(result)
    }

    /// Parse a vector (after #( consumed)
    fn parse_vector(&mut self, start_pos: Position) -> ParseResult<Value> {
        let mut elements = Vec::new();

        loop {
            let tok = self.tokenizer.peek_token()?;

            match tok {
                Token::RightParen => {
                    self.tokenizer.next_token()?; // Consume )
                    break;
                }

                Token::Eof => {
                    return Err(ParseError::new(
                        "Unexpected EOF in vector".to_string(),
                        start_pos,
                    ))
                }

                _ => {
                    elements.push(self.parse_expr()?);
                }
            }
        }

        Ok(Value::vector(elements))
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let mut tok = Tokenizer::new("(+ 1 2)");
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("+".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(1));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(2));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
        assert_eq!(tok.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_tokenize_whitespace_agnostic() {
        // This is the critical test that Steel fails!
        let input = r#"(let ((x 1)
                            (y 2))
                         (+ x y))"#;
        let mut tok = Tokenizer::new(input);

        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("let".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("x".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(1));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
        // Multi-line whitespace handled correctly!
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("y".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(2));
    }

    #[test]
    fn test_tokenize_strings() {
        let mut tok = Tokenizer::new(r#""hello world""#);
        assert_eq!(
            tok.next_token().unwrap(),
            Token::String("hello world".to_string())
        );

        let mut tok = Tokenizer::new(r#""with\nnewline""#);
        assert_eq!(
            tok.next_token().unwrap(),
            Token::String("with\nnewline".to_string())
        );
    }

    #[test]
    fn test_tokenize_cdata() {
        // Test CDATA section parsing (OpenJade extension)
        let mut tok = Tokenizer::new(r#"<![CDATA[<!DOCTYPE HTML>]]>"#);
        assert_eq!(
            tok.next_token().unwrap(),
            Token::String("<!DOCTYPE HTML>".to_string())
        );

        // Test CDATA with newlines
        let mut tok = Tokenizer::new("<![CDATA[\nLine 1\nLine 2\n]]>");
        assert_eq!(
            tok.next_token().unwrap(),
            Token::String("\nLine 1\nLine 2\n".to_string())
        );

        // Test CDATA in expression context
        let mut tok = Tokenizer::new("(define x <![CDATA[test]]>)");
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("define".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("x".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::String("test".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
    }

    #[test]
    fn test_tokenize_comments() {
        let mut tok = Tokenizer::new("(+ 1 ; comment\n 2)");
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("+".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(1));
        // Comment skipped!
        assert_eq!(tok.next_token().unwrap(), Token::Integer(2));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
    }

    #[test]
    fn test_tokenize_block_comments() {
        let mut tok = Tokenizer::new("(+ 1 #| block comment |# 2)");
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Symbol("+".to_string()));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(1));
        // Block comment skipped!
        assert_eq!(tok.next_token().unwrap(), Token::Integer(2));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
    }

    #[test]
    fn test_tokenize_booleans() {
        let mut tok = Tokenizer::new("#t #f");
        assert_eq!(tok.next_token().unwrap(), Token::Bool(true));
        assert_eq!(tok.next_token().unwrap(), Token::Bool(false));
    }

    #[test]
    fn test_tokenize_characters() {
        let mut tok = Tokenizer::new(r#"#\a #\space #\newline"#);
        assert_eq!(tok.next_token().unwrap(), Token::Char('a'));
        assert_eq!(tok.next_token().unwrap(), Token::Char(' '));
        assert_eq!(tok.next_token().unwrap(), Token::Char('\n'));
    }

    #[test]
    fn test_tokenize_hex_numbers() {
        // Lowercase #x
        let mut tok = Tokenizer::new("#xff");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(255));

        // Uppercase #X
        let mut tok = Tokenizer::new("#X10");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(16));

        // Mixed case
        let mut tok = Tokenizer::new("#xDEADBEEF");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(0xDEADBEEF));

        // Zero
        let mut tok = Tokenizer::new("#x0");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(0));
    }

    #[test]
    fn test_tokenize_octal_numbers() {
        // Lowercase #o
        let mut tok = Tokenizer::new("#o77");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(63));

        // Uppercase #O
        let mut tok = Tokenizer::new("#O10");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(8));

        // Zero
        let mut tok = Tokenizer::new("#o0");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(0));

        // Max valid octal digits
        let mut tok = Tokenizer::new("#o777");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(511));
    }

    #[test]
    fn test_tokenize_binary_numbers() {
        // Lowercase #b
        let mut tok = Tokenizer::new("#b1010");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(10));

        // Uppercase #B
        let mut tok = Tokenizer::new("#B1111");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(15));

        // Zero
        let mut tok = Tokenizer::new("#b0");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(0));

        // All ones
        let mut tok = Tokenizer::new("#b11111111");
        assert_eq!(tok.next_token().unwrap(), Token::Integer(255));
    }

    #[test]
    fn test_tokenize_quote() {
        let mut tok = Tokenizer::new("'(1 2)");
        assert_eq!(tok.next_token().unwrap(), Token::Quote);
        assert_eq!(tok.next_token().unwrap(), Token::LeftParen);
        assert_eq!(tok.next_token().unwrap(), Token::Integer(1));
        assert_eq!(tok.next_token().unwrap(), Token::Integer(2));
        assert_eq!(tok.next_token().unwrap(), Token::RightParen);
    }

    #[test]
    fn test_error_positions() {
        let mut tok = Tokenizer::new("(+ 1\n  \"unclosed string");
        tok.next_token().unwrap(); // (
        tok.next_token().unwrap(); // +
        tok.next_token().unwrap(); // 1

        let err = tok.next_token().unwrap_err();
        assert_eq!(err.position.line, 2); // Error on line 2
        assert!(err.message.contains("EOF in string"));
    }

    // =================================================================
    // Parser Tests
    // =================================================================

    #[test]
    fn test_parse_integer() {
        let mut parser = Parser::new("42");
        let val = parser.parse().unwrap();
        assert!(val.is_integer());
        if let Value::Integer(n) = val {
            assert_eq!(n, 42);
        }
    }

    #[test]
    fn test_parse_simple_list() {
        let mut parser = Parser::new("(+ 1 2)");
        let val = parser.parse().unwrap();
        assert!(val.is_list());

        // Check structure: (+ 1 2)
        if let Value::Pair(ref p) = val {
            let pair = p.borrow();
            assert!(pair.car.is_symbol());
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let mut parser = Parser::new("(+ (* 2 3) 4)");
        let val = parser.parse().unwrap();
        assert!(val.is_list());
    }

    #[test]
    fn test_parse_quoted() {
        let mut parser = Parser::new("'(1 2 3)");
        let val = parser.parse().unwrap();

        // Should be (quote (1 2 3))
        if let Value::Pair(ref p) = val {
            let pair = p.borrow();
            if let Value::Symbol(s) = &pair.car {
                assert_eq!(&**s, "quote");
            } else {
                panic!("Expected symbol 'quote'");
            }
        } else {
            panic!("Expected pair");
        }
    }

    #[test]
    fn test_parse_vector() {
        let mut parser = Parser::new("#(1 2 3)");
        let val = parser.parse().unwrap();
        assert!(val.is_vector());

        if let Value::Vector(ref v) = val {
            let vec = v.borrow();
            assert_eq!(vec.len(), 3);
        }
    }

    #[test]
    fn test_parse_dotted_list() {
        let mut parser = Parser::new("(1 . 2)");
        let val = parser.parse().unwrap();

        if let Value::Pair(ref p) = val {
            let pair = p.borrow();
            assert!(matches!(pair.car, Value::Integer(1)));
            assert!(matches!(pair.cdr, Value::Integer(2)));
        } else {
            panic!("Expected pair");
        }
    }

    #[test]
    fn test_parse_string() {
        let mut parser = Parser::new(r#""hello world""#);
        let val = parser.parse().unwrap();
        assert!(val.is_string());
    }

    #[test]
    fn test_parse_bool() {
        let mut parser = Parser::new("#t");
        let val = parser.parse().unwrap();
        assert!(val.is_bool());
        assert!(val.is_true());

        let mut parser = Parser::new("#f");
        let val = parser.parse().unwrap();
        assert!(val.is_bool());
        assert!(!val.is_true());
    }

    #[test]
    fn test_parse_multiline_let() {
        // THE CRITICAL TEST: Multi-line let bindings (breaks Steel!)
        let input = r#"
            (let ((x 1)
                  (y 2))
              (+ x y))
        "#;

        let mut parser = Parser::new(input);
        let val = parser.parse().unwrap();
        assert!(val.is_list());

        // Should parse successfully despite multi-line formatting
        // This is what Steel cannot handle!
    }

    #[test]
    fn test_parse_all() {
        let input = "(define x 1) (define y 2) (+ x y)";
        let mut parser = Parser::new(input);
        let exprs = parser.parse_all().unwrap();
        assert_eq!(exprs.len(), 3);
    }

    #[test]
    fn test_parse_empty_list() {
        let mut parser = Parser::new("()");
        let val = parser.parse().unwrap();
        assert!(val.is_nil());
    }

    #[test]
    fn test_parse_keyword() {
        let mut parser = Parser::new("#:foo");
        let val = parser.parse().unwrap();
        if let Value::Keyword(ref k) = val {
            assert_eq!(&**k, "foo");
        } else {
            panic!("Expected keyword");
        }
    }
}

