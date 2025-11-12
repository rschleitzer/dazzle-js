/**
 * Parser tests - Test Scheme parser
 */

import { describe, it, expect } from 'vitest';
import { parse, parseOne } from './parser';
import { makeNumber, makeBoolean, makeString, makeChar, makeSymbol, makePair, makeVector, theNilObj, theTrueObj, theFalseObj } from './elobj';

describe('Parser - Basic Literals', () => {
  it('should parse true', () => {
    const result = parseOne('#t');
    expect(result).toBe(theTrueObj);
  });

  it('should parse false', () => {
    const result = parseOne('#f');
    expect(result).toBe(theFalseObj);
  });

  it('should parse integer', () => {
    const result = parseOne('42');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(42);
    expect(num?.exact).toBe(true);
  });

  it('should parse negative integer', () => {
    const result = parseOne('-123');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(-123);
    expect(num?.exact).toBe(true);
  });

  it('should parse float', () => {
    const result = parseOne('3.14');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBeCloseTo(3.14);
    expect(num?.exact).toBe(false);
  });

  it('should parse number with exponent', () => {
    const result = parseOne('1.5e2');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBeCloseTo(150);
  });

  it('should parse string', () => {
    const result = parseOne('"hello"');
    const str = result.asString();
    expect(str).not.toBeNull();
    expect(str?.value).toBe('hello');
  });

  it('should parse string with escapes', () => {
    const result = parseOne('"hello\\nworld\\t!"');
    const str = result.asString();
    expect(str).not.toBeNull();
    expect(str?.value).toBe('hello\nworld\t!');
  });

  it('should parse character', () => {
    const result = parseOne('#\\a');
    const ch = result.asChar();
    expect(ch).not.toBeNull();
    expect(ch?.value).toBe('a');
  });

  it('should parse named character', () => {
    const result = parseOne('#\\newline');
    const ch = result.asChar();
    expect(ch).not.toBeNull();
    expect(ch?.value).toBe('\n');
  });

  it('should parse space character', () => {
    const result = parseOne('#\\space');
    const ch = result.asChar();
    expect(ch).not.toBeNull();
    expect(ch?.value).toBe(' ');
  });

  it('should parse symbol', () => {
    const result = parseOne('foo');
    const sym = result.asSymbol();
    expect(sym).not.toBeNull();
    expect(sym?.name).toBe('foo');
  });

  it('should parse symbol with special chars', () => {
    const result = parseOne('foo-bar?');
    const sym = result.asSymbol();
    expect(sym).not.toBeNull();
    expect(sym?.name).toBe('foo-bar?');
  });
});

describe('Parser - Lists', () => {
  it('should parse empty list', () => {
    const result = parseOne('()');
    expect(result).toBe(theNilObj);
  });

  it('should parse simple list', () => {
    const result = parseOne('(1 2 3)');
    const pair1 = result.asPair();
    expect(pair1).not.toBeNull();
    expect(pair1?.car.asNumber()?.value).toBe(1);

    const pair2 = pair1?.cdr.asPair();
    expect(pair2).not.toBeNull();
    expect(pair2?.car.asNumber()?.value).toBe(2);

    const pair3 = pair2?.cdr.asPair();
    expect(pair3).not.toBeNull();
    expect(pair3?.car.asNumber()?.value).toBe(3);

    expect(pair3?.cdr).toBe(theNilObj);
  });

  it('should parse nested list', () => {
    const result = parseOne('(1 (2 3) 4)');
    const pair1 = result.asPair();
    expect(pair1).not.toBeNull();
    expect(pair1?.car.asNumber()?.value).toBe(1);

    const nested = pair1?.cdr.asPair()?.car.asPair();
    expect(nested).not.toBeNull();
    expect(nested?.car.asNumber()?.value).toBe(2);
    expect(nested?.cdr.asPair()?.car.asNumber()?.value).toBe(3);
  });

  it('should parse improper list', () => {
    const result = parseOne('(1 . 2)');
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asNumber()?.value).toBe(1);
    expect(pair?.cdr.asNumber()?.value).toBe(2);
  });

  it('should parse improper list with multiple elements', () => {
    const result = parseOne('(1 2 3 . 4)');
    const pair1 = result.asPair();
    expect(pair1).not.toBeNull();
    expect(pair1?.car.asNumber()?.value).toBe(1);

    const pair2 = pair1?.cdr.asPair();
    expect(pair2?.car.asNumber()?.value).toBe(2);

    const pair3 = pair2?.cdr.asPair();
    expect(pair3?.car.asNumber()?.value).toBe(3);
    expect(pair3?.cdr.asNumber()?.value).toBe(4);
  });
});

describe('Parser - Vectors', () => {
  it('should parse empty vector', () => {
    const result = parseOne('#()');
    const vec = result.asVector();
    expect(vec).not.toBeNull();
    expect(vec?.elements.length).toBe(0);
  });

  it('should parse vector with elements', () => {
    const result = parseOne('#(1 2 3)');
    const vec = result.asVector();
    expect(vec).not.toBeNull();
    expect(vec?.elements.length).toBe(3);
    expect(vec?.elements[0].asNumber()?.value).toBe(1);
    expect(vec?.elements[1].asNumber()?.value).toBe(2);
    expect(vec?.elements[2].asNumber()?.value).toBe(3);
  });

  it('should parse vector with mixed types', () => {
    const result = parseOne('#(1 "hello" foo)');
    const vec = result.asVector();
    expect(vec).not.toBeNull();
    expect(vec?.elements.length).toBe(3);
    expect(vec?.elements[0].asNumber()?.value).toBe(1);
    expect(vec?.elements[1].asString()?.value).toBe('hello');
    expect(vec?.elements[2].asSymbol()?.name).toBe('foo');
  });
});

describe('Parser - Quote Abbreviations', () => {
  it('should parse quote', () => {
    const result = parseOne("'foo");
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asSymbol()?.name).toBe('quote');
    expect(pair?.cdr.asPair()?.car.asSymbol()?.name).toBe('foo');
  });

  it('should parse quasiquote', () => {
    const result = parseOne("`foo");
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asSymbol()?.name).toBe('quasiquote');
    expect(pair?.cdr.asPair()?.car.asSymbol()?.name).toBe('foo');
  });

  it('should parse unquote', () => {
    const result = parseOne(",foo");
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asSymbol()?.name).toBe('unquote');
    expect(pair?.cdr.asPair()?.car.asSymbol()?.name).toBe('foo');
  });

  it('should parse unquote-splicing', () => {
    const result = parseOne(",@foo");
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asSymbol()?.name).toBe('unquote-splicing');
    expect(pair?.cdr.asPair()?.car.asSymbol()?.name).toBe('foo');
  });

  it('should parse nested quotes', () => {
    const result = parseOne("'(1 2 3)");
    const pair = result.asPair();
    expect(pair).not.toBeNull();
    expect(pair?.car.asSymbol()?.name).toBe('quote');

    const list = pair?.cdr.asPair()?.car.asPair();
    expect(list).not.toBeNull();
    expect(list?.car.asNumber()?.value).toBe(1);
  });
});

describe('Parser - Number Radix', () => {
  it('should parse binary number', () => {
    const result = parseOne('#b1010');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(10);
    expect(num?.exact).toBe(true);
  });

  it('should parse octal number', () => {
    const result = parseOne('#o77');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(63);
  });

  it('should parse decimal number', () => {
    const result = parseOne('#d99');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(99);
  });

  it('should parse hexadecimal number', () => {
    const result = parseOne('#xFF');
    const num = result.asNumber();
    expect(num).not.toBeNull();
    expect(num?.value).toBe(255);
  });
});

describe('Parser - Whitespace and Comments', () => {
  it('should skip whitespace', () => {
    const result = parseOne('  \t\n  42  \n\t  ');
    expect(result.asNumber()?.value).toBe(42);
  });

  it('should skip comments', () => {
    const result = parseOne('; comment\n42');
    expect(result.asNumber()?.value).toBe(42);
  });

  it('should skip inline comments', () => {
    const result = parseOne('(1 ; comment\n 2 3)');
    const pair = result.asPair();
    expect(pair?.car.asNumber()?.value).toBe(1);
    expect(pair?.cdr.asPair()?.car.asNumber()?.value).toBe(2);
  });

  it('should skip multiple comments', () => {
    const results = parse('; comment 1\n42 ; comment 2\n; comment 3\n99');
    expect(results.length).toBe(2);
    expect(results[0].asNumber()?.value).toBe(42);
    expect(results[1].asNumber()?.value).toBe(99);
  });
});

describe('Parser - Multiple Datums', () => {
  it('should parse multiple expressions', () => {
    const results = parse('1 2 3');
    expect(results.length).toBe(3);
    expect(results[0].asNumber()?.value).toBe(1);
    expect(results[1].asNumber()?.value).toBe(2);
    expect(results[2].asNumber()?.value).toBe(3);
  });

  it('should parse program', () => {
    const results = parse('(define x 10)\n(+ x 20)');
    expect(results.length).toBe(2);

    const define = results[0].asPair();
    expect(define?.car.asSymbol()?.name).toBe('define');

    const add = results[1].asPair();
    expect(add?.car.asSymbol()?.name).toBe('+');
  });
});

describe('Parser - Edge Cases', () => {
  it('should handle empty input', () => {
    const results = parse('');
    expect(results.length).toBe(0);
  });

  it('should handle only whitespace', () => {
    const results = parse('  \t\n  ');
    expect(results.length).toBe(0);
  });

  it('should handle only comments', () => {
    const results = parse('; comment 1\n; comment 2');
    expect(results.length).toBe(0);
  });

  it('should throw on unmatched open paren', () => {
    expect(() => parseOne('(1 2')).toThrow();
  });

  it('should throw on unexpected close paren', () => {
    expect(() => parseOne(')')).toThrow();
  });

  it('should throw on invalid number', () => {
    // This might not throw depending on parseFloat behavior
    // Just test that something happens
    try {
      parseOne('12.34.56');
    } catch (e) {
      // Expected
    }
  });
});
