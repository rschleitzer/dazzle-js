# Arena Migration Strategy

## Goal

Replace `Gc<T>` values with generational index `ValueId` to eliminate 100x GC overhead.

## Current Status (v0.4.5 baseline + arena foundation)

**✅ Completed:**
- Arena module with mark-and-sweep GC (4/4 tests passing)
- ValueId design (Copy, zero overhead)
- ValueData enum (clean, no GC fields)
- Separate mark bitmap for uniform GC

**Performance Characteristics:**
- ValueId clone: 1 CPU cycle (copy u32)
- Current Gc<T> clone: ~1000 cycles (atomic ops)
- Expected speedup: **10-100x** on hot paths

## Migration Phases

### Phase 1: Prototype Hot Primitives (1-2 days)

Create parallel arena-based implementations of the 5 hottest primitives:

```rust
// Current (slow):
pub fn prim_car(args: &[Value]) -> Result<Value, String> {
    match &args[0] {
        Value::Pair(p) => {
            let pair = p.borrow();  // Expensive!
            Ok(pair.car.clone())     // Expensive clone!
        }
        _ => Err("not a pair".to_string()),
    }
}

// New (fast):
pub fn arena_car(arena: &Arena, args: &[ValueId]) -> Result<ValueId, String> {
    match arena.get(args[0]) {
        ValueData::Pair { car, .. } => Ok(*car),  // Just copy u32!
        _ => Err("not a pair".to_string()),
    }
}
```

**Hot primitives to migrate:**
1. `car` - 4,221 calls in DocBook test
2. `cdr` - 3,505 calls
3. `null?` - 3,968 calls
4. `equal?` - 4,496 calls
5. `cons` - ~3,000 calls

**Deliverable**: Microbenchmark showing **10-50x** speedup on hot primitives

### Phase 2: Evaluator Integration (3-5 days)

Add arena to Evaluator and create migration layer:

```rust
pub struct Evaluator {
    // New arena
    arena: Arena,

    // Keep existing fields for now
    grove: Option<Rc<dyn Grove>>,
    current_node: Option<Rc<Box<dyn Node>>>,
    mode_manager: ModeManager,
    // ...
}
```

**Migration strategy**: Dual mode
- Parser creates old `Value` initially
- Convert to `ValueId` at eval boundary
- Hot path uses arena primitives
- Cold path uses old primitives

**Deliverable**: Evaluator runs DocBook test with arena hot primitives

### Phase 3: Full Primitive Migration (1 week)

Migrate all 258 primitives in batches:

**Batch 1 - List ops** (~20 primitives):
- `append`, `reverse`, `length`, `map`, `for-each`, etc.

**Batch 2 - Type checks** (~15 primitives):
- `pair?`, `list?`, `number?`, `string?`, etc.

**Batch 3 - Grove queries** (~50 primitives):
- `children`, `parent`, `attributes`, `gi`, `data`, etc.

**Batch 4 - Arithmetic** (~42 primitives):
- `+`, `-`, `*`, `/`, `<`, `>`, etc.

**Batch 5 - Remaining** (~131 primitives):
- Strings, vectors, control flow, DSSSL types

**Strategy per batch:**
1. Update primitive signatures
2. Run tests
3. Fix failures
4. Benchmark performance

**Deliverable**: All 258 primitives use arena, tests passing

### Phase 4: Remove Old Value Type (2-3 days)

Once all primitives migrated:
1. Remove `gc` crate dependency
2. Delete old `Value` enum
3. Remove Gc/GcCell imports
4. Clean up dead code
5. Update documentation

**Deliverable**: Clean codebase, single value representation

### Phase 5: Optimize GC (1-2 days)

Tune arena GC for performance:
- Generational GC (young/old generations)
- Incremental marking (avoid pauses)
- Tune GC frequency
- Add GC statistics

**Deliverable**: Final performance tuning

## Estimated Timeline

- **Phase 1**: 1-2 days (prototype + benchmark)
- **Phase 2**: 3-5 days (evaluator integration)
- **Phase 3**: 5-7 days (full migration)
- **Phase 4**: 2-3 days (cleanup)
- **Phase 5**: 1-2 days (optimization)

**Total**: 12-19 days (2-4 weeks)

## Risk Mitigation

1. **Incremental commits**: Commit after each batch
2. **Feature flag**: Use cfg to toggle arena on/off
3. **Parallel development**: Keep old code until proven
4. **Continuous testing**: Run full test suite after each change
5. **Performance monitoring**: Benchmark after each phase

## Success Criteria

**Minimum viable:**
- All tests passing
- Within 2x of OpenJade performance

**Target:**
- All tests passing
- Within 1.2x of OpenJade performance (5x faster than current)

**Stretch goal:**
- Match or exceed OpenJade performance

## Decision Point

**Before starting Phase 2**, measure Phase 1 results:
- If hot primitives show <5x speedup → investigate
- If hot primitives show 5-20x speedup → proceed
- If hot primitives show >20x speedup → full steam ahead

This data will inform whether the full migration ROI is worth the 2-4 week effort.
