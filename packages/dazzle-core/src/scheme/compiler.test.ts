/**
 * Compiler tests - Test Scheme compiler
 */

import { describe, it, expect } from 'vitest';
import { parseOne } from './parser';
import { Compiler, Environment, GlobalEnvironment } from './compiler';
import { VM } from './vm';

describe('Compiler - Constants', () => {
  it('should compile number', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('42');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(42);
  });

  it('should compile string', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('"hello"');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asString()?.value).toBe('hello');
  });

  it('should compile boolean', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('#t');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asBoolean()?.value).toBe(true);
  });
});

describe('Compiler - Quote', () => {
  it('should compile quoted symbol', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne("'foo");
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asSymbol()?.name).toBe('foo');
  });

  it('should compile quoted list', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne("'(1 2 3)");
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    const pair = result.asPair();
    expect(pair?.car.asNumber()?.value).toBe(1);
    expect(pair?.cdr.asPair()?.car.asNumber()?.value).toBe(2);
  });
});

describe('Compiler - Primitive Calls', () => {
  it('should compile addition', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(+ 1 2 3)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(6);
  });

  it('should compile subtraction', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(- 10 3)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(7);
  });

  it('should compile nested arithmetic', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(+ (* 2 3) 4)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(10);
  });

  it('should compile list operations', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(car (cons 1 2))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(1);
  });
});

describe('Compiler - If', () => {
  it('should compile if with true condition', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(if #t 10 20)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(10);
  });

  it('should compile if with false condition', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(if #f 10 20)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(20);
  });

  it('should compile if with computed condition', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(if (< 1 2) 10 20)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(10);
  });

  it('should compile nested if', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(if #t (if #f 1 2) 3)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(2);
  });
});

describe('Compiler - Begin', () => {
  it('should compile begin', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(begin 1 2 3)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(3);
  });

  it('should compile begin with side effects', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(begin (+ 1 2) (+ 3 4))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(7);
  });
});

describe('Compiler - And/Or', () => {
  it('should compile and with all true', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(and #t #t #t)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.isTrue()).toBe(true);
  });

  it('should compile and with false', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(and #t #f #t)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.isTrue()).toBe(false);
  });

  it('should compile or with all false', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(or #f #f #f)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.isTrue()).toBe(false);
  });

  it('should compile or with true', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(or #f #t #f)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.isTrue()).toBe(true);
  });
});

describe('Compiler - Let', () => {
  it('should compile simple let', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(let ((x 10)) x)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(10);
  });

  it('should compile let with multiple bindings', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(let ((x 10) (y 20)) (+ x y))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(30);
  });

  it('should compile nested let', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(let ((x 10)) (let ((y 20)) (+ x y)))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(30);
  });

  it('should shadow outer bindings', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(let ((x 10)) (let ((x 20)) x))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(20);
  });
});

describe('Compiler - Lambda', () => {
  it('should compile lambda', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(lambda (x) x)');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asFunction()).not.toBeNull();
  });

  it('should compile lambda with body', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(lambda (x) (+ x 1))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    const func = result.asFunction();
    expect(func).not.toBeNull();
    expect(func?.signature?.nRequiredArgs).toBe(1);
  });

  it('should compile lambda with multiple parameters', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(lambda (x y) (+ x y))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    const func = result.asFunction();
    expect(func).not.toBeNull();
    expect(func?.signature?.nRequiredArgs).toBe(2);
  });
});

describe('Compiler - Complex Expressions', () => {
  it('should compile factorial-like expression', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(if (< 5 1) 1 (* 5 4))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(20);
  });

  it('should compile complex arithmetic', () => {
    const globals = new GlobalEnvironment();
    const compiler = new Compiler(globals);
    const vm = new VM();

    const expr = parseOne('(+ (* 2 3) (- 10 5))');
    const code = compiler.compile(expr, new Environment());
    const result = vm.eval(code);

    expect(result.asNumber()?.value).toBe(11);
  });
});
