//! Microbenchmark: Arena primitives vs Gc primitives
//!
//! Measures the performance difference between:
//! - Old: Gc-based Value primitives (current implementation)
//! - New: Arena-based ValueId primitives (zero-overhead)
//!
//! Expected: 10-100x speedup on hot primitives

use std::time::Instant;
use dazzle_core::scheme::arena::Arena;
use dazzle_core::scheme::arena_primitives::*;
use dazzle_core::scheme::value::Value;
use dazzle_core::scheme::primitives::*;

const ITERATIONS: usize = 1_000_000;

fn main() {
    println!("Arena vs Gc Microbenchmark");
    println!("Iterations: {}", ITERATIONS);
    println!();

    bench_car();
    bench_cdr();
    bench_cons();
    bench_null();
    bench_equal();
}

fn bench_car() {
    println!("=== car ===");

    // Setup for gc version
    let gc_pair = Value::cons(Value::Integer(1), Value::Integer(2));

    // Setup for arena version
    let mut arena = Arena::new();
    let one = arena.int(1);
    let two = arena.int(2);
    let arena_pair = arena.cons(one, two);

    // Benchmark gc version
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = prim_car(&[gc_pair.clone()]).unwrap();
    }
    let gc_time = start.elapsed();

    // Benchmark arena version
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = arena_car(&arena, &[arena_pair]).unwrap();
    }
    let arena_time = start.elapsed();

    print_results("car", gc_time, arena_time);
}

fn bench_cdr() {
    println!("=== cdr ===");

    let gc_pair = Value::cons(Value::Integer(1), Value::Integer(2));
    let mut arena = Arena::new();
    let one = arena.int(1);
    let two = arena.int(2);
    let arena_pair = arena.cons(one, two);

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = prim_cdr(&[gc_pair.clone()]).unwrap();
    }
    let gc_time = start.elapsed();

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = arena_cdr(&arena, &[arena_pair]).unwrap();
    }
    let arena_time = start.elapsed();

    print_results("cdr", gc_time, arena_time);
}

fn bench_cons() {
    println!("=== cons ===");

    let gc_one = Value::Integer(1);
    let gc_two = Value::Integer(2);

    let mut arena = Arena::new();
    let arena_one = arena.int(1);
    let arena_two = arena.int(2);

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = prim_cons(&[gc_one.clone(), gc_two.clone()]).unwrap();
    }
    let gc_time = start.elapsed();

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = arena_cons(&mut arena, &[arena_one, arena_two]).unwrap();
    }
    let arena_time = start.elapsed();

    print_results("cons", gc_time, arena_time);
}

fn bench_null() {
    println!("=== null? ===");

    let gc_nil = Value::Nil;
    let arena = Arena::new();
    let arena_nil = dazzle_core::scheme::arena::NIL_ID;

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = prim_null_p(&[gc_nil.clone()]).unwrap();
    }
    let gc_time = start.elapsed();

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = arena_null(&arena, &[arena_nil]).unwrap();
    }
    let arena_time = start.elapsed();

    print_results("null?", gc_time, arena_time);
}

fn bench_equal() {
    println!("=== equal? ===");

    // Test with nested lists: ((1 . 2) . (3 . 4))
    let gc_inner1 = Value::cons(Value::Integer(1), Value::Integer(2));
    let gc_inner2 = Value::cons(Value::Integer(3), Value::Integer(4));
    let gc_list1 = Value::cons(gc_inner1.clone(), gc_inner2.clone());
    let gc_list2 = Value::cons(gc_inner1.clone(), gc_inner2.clone());

    let mut arena = Arena::new();
    let one = arena.int(1);
    let two = arena.int(2);
    let three = arena.int(3);
    let four = arena.int(4);
    let arena_inner1 = arena.cons(one, two);
    let arena_inner2 = arena.cons(three, four);
    let arena_list1 = arena.cons(arena_inner1, arena_inner2);
    let arena_list2 = arena.cons(arena_inner1, arena_inner2);

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = prim_equal_p(&[gc_list1.clone(), gc_list2.clone()]).unwrap();
    }
    let gc_time = start.elapsed();

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = arena_equal(&arena, &[arena_list1, arena_list2]).unwrap();
    }
    let arena_time = start.elapsed();

    print_results("equal?", gc_time, arena_time);
}

fn print_results(name: &str, gc_time: std::time::Duration, arena_time: std::time::Duration) {
    let gc_ns = gc_time.as_nanos() as f64 / ITERATIONS as f64;
    let arena_ns = arena_time.as_nanos() as f64 / ITERATIONS as f64;
    let speedup = gc_ns / arena_ns;

    println!("  Gc:    {:>8.2} ns/op ({:>6.2} ms total)", gc_ns, gc_time.as_secs_f64() * 1000.0);
    println!("  Arena: {:>8.2} ns/op ({:>6.2} ms total)", arena_ns, arena_time.as_secs_f64() * 1000.0);
    println!("  Speedup: {:.1}x faster", speedup);
    println!();
}
