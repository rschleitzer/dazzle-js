/**
 * VM tests - Basic bytecode execution
 */
import { describe, it, expect } from 'vitest';
import { VM } from './vm';
import { ConstantInsn, TestInsn, PopInsn, ConsInsn, CaseInsn, FrameRefInsn, StackRefInsn, ClosureRefInsn, ReturnInsn, PrimitiveCallInsn, ClosureInsn, StackSetInsn, ClosureSetInsn, } from './insn';
import { makeNumber, makeSymbol, theTrueObj, theFalseObj, FunctionObj } from './elobj';
describe('VM', () => {
    it('should execute a constant instruction', () => {
        const vm = new VM();
        const value = makeNumber(42);
        const insn = new ConstantInsn(value, null);
        const result = vm.eval(insn);
        expect(result).toBe(value);
        expect(result.asNumber()?.value).toBe(42);
    });
    it('should execute a sequence of instructions through TestInsn', () => {
        const vm = new VM();
        // Test: if true then 10 else 20
        const consequent = new ConstantInsn(makeNumber(10), null);
        const alternative = new ConstantInsn(makeNumber(20), null);
        const test = new TestInsn(consequent, alternative);
        const pushTest = new ConstantInsn(theTrueObj, test);
        const result = vm.eval(pushTest);
        // Should take true branch
        expect(result.asNumber()?.value).toBe(10);
    });
    it('should execute a test instruction with true branch', () => {
        const vm = new VM();
        const consequent = new ConstantInsn(makeNumber(1), null);
        const alternative = new ConstantInsn(makeNumber(2), null);
        const test = new TestInsn(consequent, alternative);
        const pushTest = new ConstantInsn(theTrueObj, test);
        const result = vm.eval(pushTest);
        expect(result.asNumber()?.value).toBe(1);
    });
    it('should execute a test instruction with false branch', () => {
        const vm = new VM();
        const consequent = new ConstantInsn(makeNumber(1), null);
        const alternative = new ConstantInsn(makeNumber(2), null);
        const test = new TestInsn(consequent, alternative);
        const pushTest = new ConstantInsn(theFalseObj, test);
        const result = vm.eval(pushTest);
        expect(result.asNumber()?.value).toBe(2);
    });
    it('should execute PopInsn to remove top value', () => {
        const vm = new VM();
        // Push 42, push 99, pop, result should be 42
        const pop = new PopInsn(null);
        const push99 = new ConstantInsn(makeNumber(99), pop);
        const push42 = new ConstantInsn(makeNumber(42), push99);
        const result = vm.eval(push42);
        expect(result.asNumber()?.value).toBe(42);
    });
    it('should execute ConsInsn to create pairs', () => {
        const vm = new VM();
        // Push 1, push 2, cons -> (1 . 2)
        const cons = new ConsInsn(null);
        const push2 = new ConstantInsn(makeNumber(2), cons);
        const push1 = new ConstantInsn(makeNumber(1), push2);
        const result = vm.eval(push1);
        const pair = result.asPair();
        expect(pair).not.toBeNull();
        expect(pair?.car.asNumber()?.value).toBe(1);
        expect(pair?.cdr.asNumber()?.value).toBe(2);
    });
    it('should execute CaseInsn with matching value', () => {
        const vm = new VM();
        const matchBranch = new ConstantInsn(makeNumber(100), null);
        const failBranch = new ConstantInsn(makeNumber(200), null);
        const caseTest = new CaseInsn(makeSymbol('foo'), matchBranch, failBranch);
        const pushValue = new ConstantInsn(makeSymbol('foo'), caseTest);
        const result = vm.eval(pushValue);
        expect(result.asNumber()?.value).toBe(100);
    });
    it('should execute CaseInsn with non-matching value', () => {
        const vm = new VM();
        const matchBranch = new ConstantInsn(makeNumber(100), null);
        // Fail branch must pop the unmatched value before pushing result
        const failBranch = new PopInsn(new ConstantInsn(makeNumber(200), null));
        const caseTest = new CaseInsn(makeSymbol('foo'), matchBranch, failBranch);
        const pushValue = new ConstantInsn(makeSymbol('bar'), caseTest);
        const result = vm.eval(pushValue);
        expect(result.asNumber()?.value).toBe(200);
    });
    it('should execute CaseInsn with numeric values', () => {
        const vm = new VM();
        const matchBranch = new ConstantInsn(makeNumber(100), null);
        const failBranch = new ConstantInsn(makeNumber(200), null);
        const caseTest = new CaseInsn(makeNumber(42), matchBranch, failBranch);
        const pushValue = new ConstantInsn(makeNumber(42), caseTest);
        const result = vm.eval(pushValue);
        expect(result.asNumber()?.value).toBe(100);
    });
    it('should access frame variables with FrameRefInsn', () => {
        const vm = new VM();
        // Simple test: push 10, 20, then access frame[1]
        // Stack: [10, 20] then FrameRef[1] pushes stack[0+1]=20
        // Final stack: [10, 20, 20] - need to pop first two values
        const frameRef = new FrameRefInsn(1, null);
        const push20 = new ConstantInsn(makeNumber(20), frameRef);
        const push10 = new ConstantInsn(makeNumber(10), push20);
        // Don't use eval - test the instruction directly
        const insn = push10;
        let current = insn;
        while (current) {
            current = current.execute(vm);
        }
        // Stack should be [10, 20, 20]
        expect(vm.stackSize()).toBe(3);
        const result = vm.pop(); // Get the accessed value
        expect(result.asNumber()?.value).toBe(20);
    });
    it('should access stack variables with StackRefInsn', () => {
        const vm = new VM();
        // Push 10, 20, 30, then access sp[-2]
        // After 3 pushes, sp=3, so sp[-2] = stack[1] = 20
        const stackRef = new StackRefInsn(-2, 0, null);
        const push30 = new ConstantInsn(makeNumber(30), stackRef);
        const push20 = new ConstantInsn(makeNumber(20), push30);
        const push10 = new ConstantInsn(makeNumber(10), push20);
        // Execute directly without eval
        let current = push10;
        while (current) {
            current = current.execute(vm);
        }
        // Stack should be [10, 20, 30, 20]
        expect(vm.stackSize()).toBe(4);
        const result = vm.pop(); // Get the accessed value
        expect(result.asNumber()?.value).toBe(20);
    });
    it('should access closure variables with ClosureRefInsn', () => {
        const vm = new VM();
        // Set up a closure with captured variables
        vm.closure = [makeNumber(100), makeNumber(200), makeNumber(300)];
        // Access second closure variable (index 1)
        const closureRef = new ClosureRefInsn(1, null);
        const result = vm.eval(closureRef);
        expect(result.asNumber()?.value).toBe(200);
    });
    it('should push and pop control frames', () => {
        const vm = new VM();
        // Push some arguments on the stack
        vm.push(makeNumber(10));
        vm.push(makeNumber(20));
        vm.push(makeNumber(30));
        // Save current state
        const savedFrameIndex = vm.frameIndex;
        const savedClosure = vm.closure;
        // Create a return address
        const returnInsn = new ConstantInsn(makeNumber(999), null);
        // Push a frame (simulating function call with 3 args)
        vm.pushFrame(returnInsn, 3);
        // Change closure to simulate entering a function
        vm.closure = [makeNumber(100)];
        vm.frameIndex = vm.stackSize() - 3; // Frame points to the 3 args
        // Now pop the frame
        const next = vm.popFrame();
        // Verify state was restored
        expect(next).toBe(returnInsn);
        expect(vm.closure).toBe(savedClosure);
    });
    it('should execute ReturnInsn to return from function call', () => {
        const vm = new VM();
        // Setup: simulate a function call
        // 1. Push arguments (simulating caller pushed these)
        vm.push(makeNumber(10));
        vm.push(makeNumber(20));
        // 2. Push return address and frame
        const afterCall = new ConstantInsn(makeNumber(999), null);
        vm.pushFrame(afterCall, 2);
        // 3. Set up function's frame
        vm.frameIndex = vm.stackSize() - 2;
        // 4. Function computes result and pushes it
        vm.push(makeNumber(100)); // This is the return value
        // 5. Execute ReturnInsn (2 args to clean up)
        const returnInsn = new ReturnInsn(2);
        let current = returnInsn;
        while (current) {
            current = current.execute(vm);
        }
        // Stack should now have: [100, 999]
        // The 999 is from the afterCall instruction
        expect(vm.stackSize()).toBe(2);
        const finalValue = vm.pop();
        expect(finalValue.asNumber()?.value).toBe(999);
        const returnValue = vm.pop();
        expect(returnValue.asNumber()?.value).toBe(100);
    });
    it('should handle nested function calls with control stack', () => {
        const vm = new VM();
        // Simulate: main calls outer(), outer calls inner()
        // Main pushes args for outer: [1, 2]
        vm.push(makeNumber(1));
        vm.push(makeNumber(2));
        const mainContinue = new ConstantInsn(makeNumber(999), null);
        vm.pushFrame(mainContinue, 2);
        vm.frameIndex = vm.stackSize() - 2;
        // Outer function executes, pushes args for inner: [10, 20]
        vm.push(makeNumber(10));
        vm.push(makeNumber(20));
        const outerContinue = new ConstantInsn(makeNumber(777), null);
        vm.pushFrame(outerContinue, 2);
        vm.frameIndex = vm.stackSize() - 2;
        // Inner function computes and returns 100
        vm.push(makeNumber(100));
        const innerReturn = new ReturnInsn(2);
        // Execute inner return
        let current = innerReturn;
        current = current.execute(vm);
        // After inner return, we should be at outerContinue
        expect(current).toBe(outerContinue);
        // Stack should have: [1, 2, 100] (inner removed its 2 args: 10, 20)
        expect(vm.stackSize()).toBe(3);
        // Now simulate outer function computing its result directly
        // In a real scenario, outer would use the 100 value somehow
        // For this test, just verify we can return from outer too
        // Pop the inner result (simulating outer used it)
        vm.pop(); // 100
        // Outer computes its own result
        vm.push(makeNumber(200));
        // Outer returns
        const outerReturn = new ReturnInsn(2);
        current = outerReturn.execute(vm);
        // After outer return, we should be at mainContinue
        expect(current).toBe(mainContinue);
        // Stack should have just [200] (outer removed its 2 args: 1, 2)
        expect(vm.stackSize()).toBe(1);
        expect(vm.pop().asNumber()?.value).toBe(200);
    });
    it('should call primitive functions with PrimitiveCallInsn', () => {
        const vm = new VM();
        // Create a simple addition primitive: (+ a b)
        const addPrimitive = new FunctionObj('+', (args) => {
            if (args.length !== 2) {
                throw new Error('+ requires 2 arguments');
            }
            const a = args[0].asNumber();
            const b = args[1].asNumber();
            if (!a || !b) {
                throw new Error('+ requires numeric arguments');
            }
            return makeNumber(a.value + b.value);
        });
        // Build instruction sequence: push 10, push 20, call +
        const call = new PrimitiveCallInsn(2, addPrimitive, null);
        const push20 = new ConstantInsn(makeNumber(20), call);
        const push10 = new ConstantInsn(makeNumber(10), push20);
        const result = vm.eval(push10);
        expect(result.asNumber()?.value).toBe(30);
    });
    it('should call primitive with multiple operations', () => {
        const vm = new VM();
        // Create primitives
        const add = new FunctionObj('+', (args) => {
            const a = args[0].asNumber();
            const b = args[1].asNumber();
            return makeNumber(a.value + b.value);
        });
        const mul = new FunctionObj('*', (args) => {
            const a = args[0].asNumber();
            const b = args[1].asNumber();
            return makeNumber(a.value * b.value);
        });
        // Compute: (+ (* 3 4) 5) = (+ 12 5) = 17
        // Stack operations:
        // push 3, push 4, call *, push 5, call +
        const callAdd = new PrimitiveCallInsn(2, add, null);
        const push5 = new ConstantInsn(makeNumber(5), callAdd);
        const callMul = new PrimitiveCallInsn(2, mul, push5);
        const push4 = new ConstantInsn(makeNumber(4), callMul);
        const push3 = new ConstantInsn(makeNumber(3), push4);
        const result = vm.eval(push3);
        expect(result.asNumber()?.value).toBe(17);
    });
    it('should handle primitives with zero arguments', () => {
        const vm = new VM();
        // Create a primitive that returns a constant
        const getPi = new FunctionObj('pi', (args) => {
            if (args.length !== 0) {
                throw new Error('pi takes no arguments');
            }
            return makeNumber(3.14159);
        });
        // Call pi
        const call = new PrimitiveCallInsn(0, getPi, null);
        const result = vm.eval(call);
        expect(result.asNumber()?.value).toBeCloseTo(3.14159);
    });
    it('should create closures with ClosureInsn', () => {
        const vm = new VM();
        // Create a simple closure that returns a constant
        // (lambda () 42)
        const lambdaBody = new ConstantInsn(makeNumber(42), null);
        const signature = {
            nRequiredArgs: 0,
            nOptionalArgs: 0,
            restArg: false,
            nKeyArgs: 0,
        };
        const createClosure = new ClosureInsn(signature, lambdaBody, 0, null);
        const result = vm.eval(createClosure);
        // Result should be a function object
        const func = result.asFunction();
        expect(func).not.toBeNull();
        expect(func?.isClosure()).toBe(true);
        expect(func?.code).toBe(lambdaBody);
    });
    it('should create closures with captured variables', () => {
        const vm = new VM();
        // Create a closure that captures one variable
        // Simulate: (let ((x 10)) (lambda () x))
        // Push 10 onto stack (this will be captured)
        // Then create closure with display length 1
        const lambdaBody = new ClosureRefInsn(0, null); // Reference captured variable at index 0
        const signature = {
            nRequiredArgs: 0,
            nOptionalArgs: 0,
            restArg: false,
            nKeyArgs: 0,
        };
        const createClosure = new ClosureInsn(signature, lambdaBody, 1, null);
        const pushX = new ConstantInsn(makeNumber(10), createClosure);
        const result = vm.eval(pushX);
        // Result should be a closure with a display
        const func = result.asFunction();
        expect(func).not.toBeNull();
        expect(func?.isClosure()).toBe(true);
        expect(func?.display).toBeDefined();
        expect(func?.display?.length).toBe(1);
        expect(func?.display?.[0].asNumber()?.value).toBe(10);
    });
    it('should create closures with multiple captured variables', () => {
        const vm = new VM();
        // Create a closure that captures two variables
        // Simulate: (let ((x 10) (y 20)) (lambda () ...))
        // The lambda can reference both x and y
        const lambdaBody = new ConstantInsn(makeNumber(0), null); // Dummy body
        const signature = {
            nRequiredArgs: 0,
            nOptionalArgs: 0,
            restArg: false,
            nKeyArgs: 0,
        };
        const createClosure = new ClosureInsn(signature, lambdaBody, 2, null);
        const pushY = new ConstantInsn(makeNumber(20), createClosure);
        const pushX = new ConstantInsn(makeNumber(10), pushY);
        const result = vm.eval(pushX);
        // Verify the closure captured both variables
        const func = result.asFunction();
        expect(func?.display?.length).toBe(2);
        expect(func?.display?.[0].asNumber()?.value).toBe(10);
        expect(func?.display?.[1].asNumber()?.value).toBe(20);
    });
    it('should mutate stack variables with StackSetInsn', () => {
        const vm = new VM();
        // Push values: 10, 20, 30
        vm.push(makeNumber(10));
        vm.push(makeNumber(20));
        vm.push(makeNumber(30));
        // Push new value 99 and set stack[-2]
        // Stack before set: [10, 20, 30, 99], sp=4
        // stack[-2] = stack[4-2] = stack[2] = 30
        const setStack = new StackSetInsn(-2, 0, null);
        const push99 = new ConstantInsn(makeNumber(99), setStack);
        let current = push99;
        while (current) {
            current = current.execute(vm);
        }
        // Stack should be: [10, 20, 99, 30]
        // The old value (30) is left on top
        expect(vm.stackSize()).toBe(4);
        expect(vm.pop().asNumber()?.value).toBe(30); // old value
        expect(vm.pop().asNumber()?.value).toBe(99); // new value set here
        expect(vm.pop().asNumber()?.value).toBe(20);
        expect(vm.pop().asNumber()?.value).toBe(10);
    });
    it('should mutate closure variables with ClosureSetInsn', () => {
        const vm = new VM();
        // Set up a closure with two variables: [10, 20]
        vm.closure = [makeNumber(10), makeNumber(20)];
        // Push new value 99 and set closure[1] (which is 20)
        const setClosure = new ClosureSetInsn(1, null);
        const push99 = new ConstantInsn(makeNumber(99), setClosure);
        let current = push99;
        while (current) {
            current = current.execute(vm);
        }
        // Closure should now be: [10, 99]
        expect(vm.closure?.[0].asNumber()?.value).toBe(10);
        expect(vm.closure?.[1].asNumber()?.value).toBe(99);
        // Old value (20) should be on stack
        expect(vm.stackSize()).toBe(1);
        expect(vm.pop().asNumber()?.value).toBe(20);
    });
    it('should handle set! semantics with mutations', () => {
        const vm = new VM();
        // Simulate basic mutation: push x=10, push newval=20, set x to newval
        // Stack: [10, 20]
        // After set stack[-2]: [20, 10]  (10 is the old value returned)
        vm.push(makeNumber(10)); // x = 10
        vm.push(makeNumber(20)); // new value
        const setX = new StackSetInsn(-2, 0, null);
        let current = setX;
        while (current) {
            current = current.execute(vm);
        }
        // Stack: [20, 10]
        // x was mutated to 20, old value 10 is on top
        expect(vm.stackSize()).toBe(2);
        expect(vm.pop().asNumber()?.value).toBe(10); // old value
        expect(vm.peek().asNumber()?.value).toBe(20); // mutated value still at stack[0]
    });
});
//# sourceMappingURL=vm.test.js.map