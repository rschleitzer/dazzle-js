# Performance Analysis: Dazzle vs OpenJade

## Executive Summary

Dazzle is **74x slower per eval call** than OpenJade (1.34ms vs 0.018ms). The bottleneck is the Scheme interpreter architecture, not XML/grove operations.

## Benchmark Results

### dsssl.xml Processing (954KB, 16K lines, 12,925 elements)

**Dazzle:**
- Total time: 70.32s
- XML parsing: 0.04s (0.06%)
- Template loading: 1.04s (1.5%)
- Template evaluation: 0.18s (0.3%)
- **DSSSL processing: 66.78s (97%)**

**OpenJade:**
- Total time: 0.89s
- **79x faster overall**

### DSSSL Processing Breakdown

**Dazzle internals:**
- `process_node()` calls: 17,500
- `eval()` calls: 50,000 (2.9x ratio - normal)
- Grove operations (gi, parent, children): **<0.001%**
- Rule matching (HashMap lookup): **<0.001%**
- **Scheme interpreter (eval): 100%**

**Performance per operation:**
- Dazzle: 1.34ms per eval
- OpenJade: ~0.018ms per eval
- **74x slower per evaluation**

## Root Cause: Interpreter Architecture

### Dazzle (Tree-Walking Interpreter)

```rust
pub fn eval(&mut self, expr: Value, env: Gc<Environment>) -> EvalResult {
    match expr {
        Value::Symbol(sym) => env.lookup(&sym),  // Hash lookup
        Value::Pair(_) => {
            match first {
                Special("if") => eval_if(...),    // Recursive
                Special("lambda") => eval_lambda(...),
                _ => apply(eval(func), eval_args(...))  // More recursion
            }
        }
        _ => Ok(expr)  // Self-evaluating
    }
}
```

**Overhead per eval:**
1. Function call overhead
2. Pattern matching on Value enum
3. Environment hash lookups
4. Recursive calls for subexpressions
5. GC pressure from temporary allocations
6. Context saving/restoring

### OpenJade (Bytecode VM)

```cpp
ELObj *VM::eval(const Insn *insn, ELObj **display, ELObj *arg) {
    initStack();
    // The inner loop.
    while (insn)
        insn = insn->execute(*this);  // Just a virtual call!
    return *sp;
}

// Example instruction:
const Insn *ConstantInsn::execute(VM &vm) const {
    vm.needStack(1);
    *vm.sp++ = value_;        // Push to stack
    return next_.pointer();    // Next instruction
}
```

**Advantages:**
1. **No recursion** - simple while loop
2. **No pattern matching** - direct virtual dispatch
3. **Pre-resolved closures** - no environment lookup
4. **Stack-based** - minimal GC pressure
5. **Cache-friendly** - tight instruction loop
6. **Compiled once, run many times** - `InsnPtr insn_` cached in rules

## Key Architectural Differences

### 1. Execution Model

**Dazzle:**
- Interprets AST directly (tree-walking)
- Recursive `eval()` calls
- ~50K function calls for dsssl.xml

**OpenJade:**
- Compiles AST to instructions once
- Flat while loop executes instructions
- Minimal function call overhead

### 2. Value Storage

**Dazzle:**
- `enum Value` - pattern matching overhead
- Gc<> wrappers for GC
- Allocations for each operation

**OpenJade:**
- `ELObj*` pointers - direct dispatch
- Custom garbage collector (Collector::Object)
- Stack-based evaluation reduces allocations

### 3. Closure Representation

**Dazzle:**
- `Gc<Environment>` - hash map lookups
- Environment chain traversal
- Clone environments for closures

**OpenJade:**
- `ELObj **display` - direct pointer array
- Pre-computed closure offsets in instructions
- No lookup needed during execution

### 4. Compilation Strategy

**Dazzle:**
- No compilation phase
- Interprets `Value` AST every time
- Rule expressions evaluated directly

**OpenJade:**
```cpp
class Identifier {
    Owner<Expression> def_;   // Parsed AST
    InsnPtr insn_;            // Compiled instructions (cached!)
    ELObj *value_;            // Computed value (if constant)
};
```
- Expressions compiled to `InsnPtr` once
- Instructions cached in identifier/rule
- Subsequent evaluations use cached instructions

**Full VM Port** - Replicate OpenJade's design
   - Virtual machine with instruction dispatch
   - Stack-based with control stack
   - Pre-compiled closure offsets
   - Custom GC tuned for Scheme
   - Impact: Match OpenJade performance

Port OpenJade's VM architecture completely:
- Instruction-based execution
- Stack-based evaluation
- Pre-compiled closures
- Custom GC

**Effort:** 2-3 months
**Expected gain:** 50-74x speedup (match OpenJade)
**Trade-off:** Significant complexity increase

## Conclusion

The 74x performance gap is due to **fundamental interpreter architecture**, not implementation bugs:

- **Tree-walking** (Dazzle) vs **Bytecode VM** (OpenJade)
- **Recursive eval** vs **Flat instruction loop**
- **Runtime interpretation** vs **Compile-once, run-many**


---

**Analysis Date:** 2025-11-11
**Test Case:** dsssl.xml (954KB, 12,925 elements)
**Dazzle Version:** 0.4.5
**OpenJade Version:** 1.3.2
