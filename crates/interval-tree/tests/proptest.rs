#![expect(unused_qualifications)]
#![cfg(not(miri))]
use interval_tree::{IntervalTree, TextRange};
use proptest::prelude::*;
use proptest_derive::Arbitrary;

mod simple;
use simple::SimpleIntervalMap;

// Limit ranges to u16::MAX to avoid massive allocations
const MAX_POS: usize = u16::MAX as usize;

#[derive(Arbitrary, Debug, Clone)]
struct Insert {
    start: u16,
    end: u16,
    value: i32,
}

#[derive(Arbitrary, Debug, Clone)]
struct Delete {
    start: u16,
    end: u16,
    del_extend: bool,
}

#[derive(Arbitrary, Debug, Clone)]
struct FindIntersects {
    start: u16,
    end: u16,
}

#[derive(Arbitrary, Debug, Clone)]
struct Advance {
    position: u16,
    length: u16,
}

#[derive(Arbitrary, Debug, Clone)]
struct ApplyWithSplit {
    start: u16,
    end: u16,
    multiply_by: i32,
}

#[derive(Arbitrary, Debug, Clone)]
enum Operation {
    Insert(Insert),
    Delete(Delete),
    FindIntersects(FindIntersects),
    Advance(Advance),
    ApplyWithSplit(ApplyWithSplit),
}

fn make_range(start: u16, end: u16) -> TextRange {
    let start = start as usize;
    let end = end as usize;
    if start > end {
        TextRange::new(end, start)
    } else {
        TextRange::new(start, end)
    }
}

fn insert_both<F: Fn(Vec<i32>, Vec<i32>) -> Vec<i32>>(
    tree: &mut IntervalTree<Vec<i32>>,
    simple: &mut SimpleIntervalMap<Vec<i32>>,
    range: TextRange,
    value: Vec<i32>,
    merge_fn: F,
) {
    tree.insert(range, value.clone(), &merge_fn);
    simple.insert(range, value, merge_fn);
}

fn delete_both(
    tree: &mut IntervalTree<Vec<i32>>,
    simple: &mut SimpleIntervalMap<Vec<i32>>,
    range: TextRange,
    del_extend: bool,
) {
    tree.delete(range, del_extend);
    simple.delete(range, del_extend);
}

fn advance_both(
    tree: &mut IntervalTree<Vec<i32>>,
    simple: &mut SimpleIntervalMap<Vec<i32>>,
    position: usize,
    length: usize,
) {
    tree.advance(position, length);
    simple.advance(position, length);
}

fn apply_with_split_both<F: Fn(Vec<i32>) -> Option<Vec<i32>> + Clone>(
    tree: &mut IntervalTree<Vec<i32>>,
    simple: &mut SimpleIntervalMap<Vec<i32>>,
    range: TextRange,
    f: F,
) {
    tree.apply_with_split(f.clone(), range);
    simple.apply_with_split(f, range);
}

// Compare tree and simple implementations for consistency
fn compare_find_intersects(
    tree: &IntervalTree<Vec<i32>>,
    simple: &SimpleIntervalMap<Vec<i32>>,
    range: TextRange,
) {
    let tree_results: Vec<_> =
        tree.find_intersects(range).map(|n| (n.key, n.val.clone())).collect();
    let simple_results = simple.find_intersects(range);

    // Both should find the same intervals with the same values
    assert_eq!(
        tree_results.len(),
        simple_results.len(),
        "Different number of intersects for range {range:?}. Tree: {tree_results:?}, Simple: {simple_results:?}"
    );

    for ((tree_range, tree_val), (simple_range, simple_val)) in
        tree_results.iter().zip(&simple_results)
    {
        assert_eq!(tree_range, simple_range, "Different ranges");
        assert_eq!(tree_val, simple_val, "Different values for range {tree_range:?}");
    }
}

fn compare_get(tree: &IntervalTree<Vec<i32>>, simple: &SimpleIntervalMap<Vec<i32>>, pos: usize) {
    let tree_val = tree.find(pos).map(|n| n.val.clone());
    let simple_val = simple.find(pos);
    assert_eq!(tree_val, simple_val, "Different values at position {pos}");
}

fn compare_size(tree: &IntervalTree<Vec<i32>>, simple: &SimpleIntervalMap<Vec<i32>>) {
    // Note: tree.size() counts intervals, simple.size() counts positions with values
    // We can't directly compare these, but we can ensure they're consistent with content
    let tree_size = tree.size();
    let simple_size = simple.size();

    // Both should be 0 if empty
    if tree_size == 0 {
        assert_eq!(simple_size, 0, "Tree is empty but simple is not");
    }
    if simple_size == 0 {
        assert_eq!(tree_size, 0, "Simple is empty but tree is not");
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 10,
        failure_persistence: Some(Box::new(proptest::test_runner::FileFailurePersistence::WithSource("proptest-regressions"))),
        ..ProptestConfig::default()
    })]

    #[test]
    fn pt_single_insert(insert in any::<Insert>()) {
        let range = make_range(insert.start, insert.end);
        if range.start == range.end {
            return Ok(()); // Skip empty ranges
        }

        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        insert_both(&mut tree, &mut simple, range, vec![insert.value], |mut new, mut old| {
            new.append(&mut old);
            new
        });

        // Check that both have the same content
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));
        compare_size(&tree, &simple);
    }

    #[test]
    fn pt_single_delete(
        insert in any::<Insert>(),
        delete in any::<Delete>()
    ) {
        let insert_range = make_range(insert.start, insert.end);
        let delete_range = make_range(delete.start, delete.end);

        if insert_range.start == insert_range.end || delete_range.start == delete_range.end {
            return Ok(()); // Skip empty ranges
        }

        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        // Insert then delete
        insert_both(&mut tree, &mut simple, insert_range, vec![insert.value], |mut new, mut old| {
            new.append(&mut old);
            new
        });
        delete_both(&mut tree, &mut simple, delete_range, delete.del_extend);

        // Verify consistency
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));
        compare_size(&tree, &simple);
    }

    #[test]
    fn pt_single_advance(
        insert in any::<Insert>(),
        advance in any::<Advance>()
    ) {
        let range = make_range(insert.start, insert.end);
        if range.start == range.end {
            return Ok(()); // Skip empty ranges
        }

        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        // Insert then advance
        insert_both(&mut tree, &mut simple, range, vec![insert.value], |mut new, mut old| {
            new.append(&mut old);
            new
        });
        advance_both(&mut tree, &mut simple, advance.position as usize, advance.length as usize);

        // Verify consistency
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));
        compare_size(&tree, &simple);
    }

    #[test]
    fn pt_merge_overlapping(
        insert1 in any::<Insert>(),
        insert2 in any::<Insert>()
    ) {
        let range1 = make_range(insert1.start, insert1.end);
        let range2 = make_range(insert2.start, insert2.end);

        if range1.start == range1.end || range2.start == range2.end {
            return Ok(()); // Skip empty ranges
        }

        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        let merge_fn = |mut new: Vec<i32>, mut old: Vec<i32>| {
            new.append(&mut old);
            new
        };

        // Insert overlapping intervals
        insert_both(&mut tree, &mut simple, range1, vec![insert1.value], merge_fn);
        insert_both(&mut tree, &mut simple, range2, vec![insert2.value], merge_fn);

        // Verify consistency
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));

        // Test specific intersections
        if range1.intersects(range2) {
            compare_find_intersects(&tree, &simple, range1);
            compare_find_intersects(&tree, &simple, range2);
        }
    }

    #[test]
    fn pt_apply_with_split(
        insert in any::<Insert>(),
        apply in any::<ApplyWithSplit>()
    ) {
        let insert_range = make_range(insert.start, insert.end);
        let apply_range = make_range(apply.start, apply.end);

        if insert_range.start == insert_range.end || apply_range.start == apply_range.end {
            return Ok(()); // Skip empty ranges
        }

        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        // Insert then apply transformation
        insert_both(&mut tree, &mut simple, insert_range, vec![insert.value], |mut new, mut old| {
            new.append(&mut old);
            new
        });

        let multiply_by = apply.multiply_by;
        apply_with_split_both(
            &mut tree,
            &mut simple,
            apply_range,
            move |mut val| {
                if multiply_by == 0 {
                    None // Remove the value
                } else {
                    // Multiply each element in the vector
                    for v in &mut val {
                        *v = v.wrapping_mul(multiply_by);
                    }
                    Some(val)
                }
            }
        );

        // Verify consistency
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));
        compare_size(&tree, &simple);
    }

    #[test]
    fn pt_combo_operations(operations in prop::collection::vec(any::<Operation>(), 0..20)) {
        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        for op in operations {
            match op {
                Operation::Insert(insert) => {
                    let range = make_range(insert.start, insert.end);
                    if range.start != range.end {
                        insert_both(&mut tree, &mut simple, range, vec![insert.value], |mut new: Vec<i32>, mut old: Vec<i32>| {
                            new.append(&mut old);
                            new
                        });
                    }
                }
                Operation::Delete(delete) => {
                    let range = make_range(delete.start, delete.end);
                    if range.start != range.end {
                        delete_both(&mut tree, &mut simple, range, delete.del_extend);
                    }
                }
                Operation::FindIntersects(find) => {
                    let range = make_range(find.start, find.end);
                    if range.start != range.end {
                        compare_find_intersects(&tree, &simple, range);
                    }
                }
                Operation::Advance(advance) => {
                    if advance.length > 0 {
                        advance_both(&mut tree, &mut simple, advance.position as usize, advance.length as usize);
                    }
                }
                Operation::ApplyWithSplit(apply) => {
                    let range = make_range(apply.start, apply.end);
                    if range.start != range.end {
                        let multiply_by = apply.multiply_by.max(1); // Avoid 0
                        apply_with_split_both(
                            &mut tree,
                            &mut simple,
                            range,
                            move |mut val| {
                                // Multiply each element in the vector
                                for v in &mut val {
                                    *v = v.wrapping_mul(multiply_by);
                                }
                                Some(val)
                            }
                        );
                    }
                }
            }

            // After each operation, verify consistency
            compare_size(&tree, &simple);
        }

        // Final comprehensive check
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));

        // Test some random position lookups
        for pos in [0, 100, 1000, MAX_POS / 2, MAX_POS - 1] {
            compare_get(&tree, &simple, pos);
        }
    }

    #[test]
    fn pt_clean_operations(
        inserts in prop::collection::vec(any::<Insert>(), 0..10),
        empty_value in any::<i32>()
    ) {
        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        // Insert multiple intervals
        for insert in inserts {
            let range = make_range(insert.start, insert.end);
            if range.start != range.end {
                insert_both(&mut tree, &mut simple, range, vec![insert.value], |mut new: Vec<i32>, mut old: Vec<i32>| {
                    new.append(&mut old);
                    new
                });
            }
        }

        // Clean both structures
        let eq_fn = |a: &Vec<i32>, b: &Vec<i32>| a == b;
        let empty_fn = |v: &Vec<i32>| v.contains(&empty_value);

        tree.clean(eq_fn, empty_fn);
        simple.clean(eq_fn, empty_fn);

        // Verify consistency after cleaning
        compare_find_intersects(&tree, &simple, TextRange::new(0, MAX_POS));
        compare_size(&tree, &simple);
    }

    #[test]
    fn pt_find_intersect_min_max(
        inserts in prop::collection::vec(any::<Insert>(), 0..5),
        query in any::<FindIntersects>()
    ) {
        let mut tree = IntervalTree::new();
        let mut simple = SimpleIntervalMap::new();

        // Insert intervals
        for insert in inserts {
            let range = make_range(insert.start, insert.end);
            if range.start != range.end {
                insert_both(&mut tree, &mut simple, range, vec![insert.value], |mut new, mut old| {
                    new.append(&mut old);
                    new
                });
            }
        }

        let query_range = make_range(query.start, query.end);
        if query_range.start == query_range.end {
            return Ok(()); // Skip empty ranges
        }

        // Test find_intersect_min
        let tree_min = tree.find_intersect_min(query_range);
        let simple_intersects = simple.find_intersects(query_range);
        let simple_min = simple_intersects.first();

        match (tree_min, simple_min) {
            (Some(tree_node), Some((simple_range, simple_val))) => {
                assert_eq!(tree_node.key, *simple_range, "Different min ranges");
                assert_eq!(&tree_node.val, simple_val, "Different min values");
            }
            (None, None) => {} // Both found nothing - OK
            (tree_result, simple_result) => {
                panic!("Inconsistent find_intersect_min results. Tree: {:?}, Simple: {:?}",
                       tree_result.map(|n| (n.key, &n.val)), simple_result);
            }
        }

        // Test find_intersect_max
        let tree_max = tree.find_intersect_max(query_range);
        let simple_max = simple_intersects.last();

        match (tree_max, simple_max) {
            (Some(tree_node), Some((simple_range, simple_val))) => {
                assert_eq!(tree_node.key, *simple_range, "Different max ranges");
                assert_eq!(&tree_node.val, simple_val, "Different max values");
            }
            (None, None) => {} // Both found nothing - OK
            (tree_result, simple_result) => {
                panic!("Inconsistent find_intersect_max results. Tree: {:?}, Simple: {:?}",
                       tree_result.map(|n| (n.key, &n.val)), simple_result);
            }
        }
    }
}
