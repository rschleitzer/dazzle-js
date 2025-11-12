//! Bridge between old Value (Gc-based) and new arena (ValueId-based) systems
//!
//! This module provides conversion functions to allow gradual migration from
//! the tree-walking interpreter (using Value with Gc) to the bytecode VM
//! (using ValueData with Arena).
//!
//! ## Migration Strategy
//!
//! 1. Parse to old Value system (existing parser works)
//! 2. Convert Value → ValueId (this module)
//! 3. Compile ValueId to bytecode (compiler.rs)
//! 4. Execute bytecode (vm.rs)
//! 5. Convert result ValueId → Value (this module)
//! 6. Return to existing code
//!
//! This allows us to get VM performance while keeping existing code working.

use crate::scheme::arena::{Arena, ProcedureData, ValueData, ValueId, NIL_ID, TRUE_ID, FALSE_ID};
use crate::scheme::environment::Environment;
use crate::scheme::value::{Procedure, Value};
use gc::Gc;

/// Convert a Value to ValueId, allocating in the arena
pub fn value_to_arena(arena: &mut Arena, value: &Value) -> ValueId {
    match value {
        Value::Nil => NIL_ID,
        Value::Bool(true) => TRUE_ID,
        Value::Bool(false) => FALSE_ID,
        Value::Integer(n) => arena.int(*n),
        Value::Real(f) => arena.real(*f),
        Value::Quantity { magnitude, unit } => {
            arena.alloc(ValueData::Quantity { magnitude: *magnitude, unit: *unit })
        }
        Value::Char(c) => arena.char(*c),
        Value::String(s) => arena.string(s.as_ref().clone()),
        Value::Symbol(sym) => arena.symbol(sym.clone()),
        Value::Keyword(kw) => arena.keyword(kw.clone()),
        Value::Pair(pair) => {
            let pair_data = pair.borrow();
            let car = value_to_arena(arena, &pair_data.car);
            let cdr = value_to_arena(arena, &pair_data.cdr);
            if let Some(pos) = &pair_data.pos {
                arena.cons_with_pos(car, cdr, pos.clone())
            } else {
                arena.cons(car, cdr)
            }
        }
        Value::Vector(vec) => {
            let elements = vec.borrow();
            let arena_elements: Vec<ValueId> = elements
                .iter()
                .map(|v| value_to_arena(arena, v))
                .collect();
            arena.vector(arena_elements)
        }
        Value::Procedure(proc) => {
            convert_procedure_to_arena(arena, proc)
        }
        Value::Node(node) => {
            arena.alloc(ValueData::Node(node.clone()))
        }
        Value::NodeList(nl) => {
            arena.alloc(ValueData::NodeList(nl.clone()))
        }
        Value::Sosofo => {
            arena.alloc(ValueData::Sosofo)
        }
        Value::Unspecified => {
            arena.alloc(ValueData::Unspecified)
        }
        Value::Error => {
            arena.alloc(ValueData::Error)
        }
    }
}

/// Stub function for primitives that can't be converted
fn primitive_stub(_arena: &mut Arena, _args: &[ValueId]) -> Result<ValueId, String> {
    Err("Primitive must be registered in VM, not converted from old Value".to_string())
}

/// Convert a Procedure to ProcedureData in the arena
fn convert_procedure_to_arena(arena: &mut Arena, proc: &Procedure) -> ValueId {
    match proc {
        Procedure::Primitive { name, .. } => {
            // Can't convert old primitive signature to new - just create a stub
            // In practice, primitives should be registered directly in the VM
            arena.alloc(ValueData::Procedure(ProcedureData::Primitive {
                name,
                func: primitive_stub,
            }))
        }
        Procedure::Lambda {
            params,
            required_count,
            optional_defaults,
            body,
            env: _,
            source,
            name,
        } => {
            // Convert body and defaults to arena
            let body_id = value_to_arena(arena, body);
            let defaults: Vec<ValueId> = optional_defaults
                .iter()
                .map(|v| value_to_arena(arena, v))
                .collect();

            // For now, store env as NIL - full closure conversion is complex
            // TODO: Convert environment properly
            let env_id = NIL_ID;

            arena.alloc(ValueData::Procedure(ProcedureData::Lambda {
                params: (**params).clone(),
                required_count: *required_count,
                optional_defaults: defaults,
                body: body_id,
                env: env_id,
                source: source.clone(),
                name: name.clone(),
            }))
        }
    }
}

/// Convert a ValueId back to Value (for returning to old code)
pub fn arena_to_value(arena: &Arena, value_id: ValueId) -> Value {
    let data = arena.get(value_id);

    match data {
        ValueData::Nil => Value::Nil,
        ValueData::Bool(b) => Value::Bool(*b),
        ValueData::Integer(n) => Value::Integer(*n),
        ValueData::Real(f) => Value::Real(*f),
        ValueData::Quantity { magnitude, unit } => {
            Value::Quantity { magnitude: *magnitude, unit: *unit }
        }
        ValueData::Char(c) => Value::Char(*c),
        ValueData::String(s) => Value::String(Gc::new(s.clone())),
        ValueData::Symbol(sym) => Value::Symbol(sym.clone()),
        ValueData::Keyword(kw) => Value::Keyword(kw.clone()),
        ValueData::Pair { car, cdr, pos } => {
            let car_val = arena_to_value(arena, *car);
            let cdr_val = arena_to_value(arena, *cdr);
            if let Some(p) = pos {
                Value::cons_with_pos(car_val, cdr_val, p.clone())
            } else {
                Value::cons(car_val, cdr_val)
            }
        }
        ValueData::Vector(elements) => {
            let values: Vec<Value> = elements
                .iter()
                .map(|&id| arena_to_value(arena, id))
                .collect();
            Value::vector(values)
        }
        ValueData::Procedure(proc_data) => {
            convert_procedure_from_arena(arena, proc_data)
        }
        ValueData::Node(node) => {
            Value::Node(node.clone())
        }
        ValueData::NodeList(nl) => {
            Value::NodeList(nl.clone())
        }
        ValueData::Sosofo => Value::Sosofo,
        ValueData::Unspecified => Value::Unspecified,
        ValueData::Error => Value::Error,
    }
}

/// Convert ProcedureData back to Procedure
fn convert_procedure_from_arena(arena: &Arena, proc_data: &ProcedureData) -> Value {
    match proc_data {
        ProcedureData::Primitive { name, .. } => {
            // Return a placeholder - we can't easily convert back
            // In practice, primitives should stay in their native form
            Value::primitive(name, |_| Err("Arena primitive called in Gc context".to_string()))
        }
        ProcedureData::Lambda {
            params,
            required_count,
            optional_defaults,
            body,
            env: _,
            source,
            name,
        } => {
            let body_val = arena_to_value(arena, *body);
            let defaults: Vec<Value> = optional_defaults
                .iter()
                .map(|&id| arena_to_value(arena, id))
                .collect();

            // Create a dummy environment - proper conversion is complex
            let env = Environment::new_global();

            Value::lambda_with_optional(
                params.clone(),
                *required_count,
                defaults,
                body_val,
                env,
                source.clone(),
                name.clone(),
            )
        }
        ProcedureData::CompiledLambda { params, required_count: _, name, .. } => {
            // Can't easily convert compiled lambda back
            // Return a stub that reports the issue
            let env = Environment::new_global();
            Value::lambda_with_source(
                params.clone(),
                Value::symbol("compiled-lambda-stub"),
                env,
                None,
                name.clone(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_simple_values() {
        let mut arena = Arena::new();

        // Test nil
        let nil = Value::Nil;
        let nil_id = value_to_arena(&mut arena, &nil);
        assert_eq!(nil_id, NIL_ID);
        let nil_back = arena_to_value(&arena, nil_id);
        assert!(matches!(nil_back, Value::Nil));

        // Test boolean
        let bool_true = Value::Bool(true);
        let bool_id = value_to_arena(&mut arena, &bool_true);
        assert_eq!(bool_id, TRUE_ID);

        // Test integer
        let int = Value::Integer(42);
        let int_id = value_to_arena(&mut arena, &int);
        let int_back = arena_to_value(&arena, int_id);
        assert!(matches!(int_back, Value::Integer(42)));

        // Test string
        let string = Value::String(Gc::new("hello".to_string()));
        let string_id = value_to_arena(&mut arena, &string);
        let string_back = arena_to_value(&arena, string_id);
        if let Value::String(s) = string_back {
            assert_eq!(s.as_ref(), "hello");
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_convert_pair() {
        let mut arena = Arena::new();

        // Build (1 . 2)
        let pair = Value::cons(Value::Integer(1), Value::Integer(2));
        let pair_id = value_to_arena(&mut arena, &pair);
        let pair_back = arena_to_value(&arena, pair_id);

        if let Value::Pair(p) = pair_back {
            let pd = p.borrow();
            assert!(matches!(pd.car, Value::Integer(1)));
            assert!(matches!(pd.cdr, Value::Integer(2)));
        } else {
            panic!("Expected pair");
        }
    }

    #[test]
    fn test_convert_list() {
        let mut arena = Arena::new();

        // Build (1 2 3)
        let list = Value::cons(
            Value::Integer(1),
            Value::cons(
                Value::Integer(2),
                Value::cons(Value::Integer(3), Value::Nil),
            ),
        );

        let list_id = value_to_arena(&mut arena, &list);
        let list_back = arena_to_value(&arena, list_id);

        // Verify structure
        if let Value::Pair(p1) = list_back {
            let pd1 = p1.borrow();
            assert!(matches!(pd1.car, Value::Integer(1)));
            if let Value::Pair(p2) = &pd1.cdr {
                let pd2 = p2.borrow();
                assert!(matches!(pd2.car, Value::Integer(2)));
            } else {
                panic!("Expected pair for cdr");
            }
        } else {
            panic!("Expected pair");
        }
    }
}
