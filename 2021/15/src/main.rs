use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashSet};
use std::iter::repeat;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
struct Node {
    x: usize,
    y: usize,
    danger: u32,
    dist: u32,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .dist
            .cmp(&self.dist)
            .then_with(|| other.danger.cmp(&self.danger))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let (dims, input) = make_dists();
    let mut dists = BinaryHeap::from(input);
    let mut seen = HashSet::new();

    while let Some(n) = dists.pop() {
        seen.insert(n);

        update_neighbors(n, &mut dists);
    }

    println!(
        "{}",
        seen.into_iter()
            .find(|n| n.x == dims.0 && n.y == dims.1)
            .unwrap()
            .dist
    );
}

fn update_neighbors(node: Node, neighbors: &mut BinaryHeap<Node>) {
    *neighbors = neighbors.iter().copied().map(|mut n| {
            if n.danger + node.dist > n.dist {
                return n;
            }

            if (n.x == node.x && node.y > 0 && n.y == node.y - 1)
                || (n.x == node.x && n.y == node.y + 1)
                || (n.y == node.y && node.x > 0 && n.x == node.x - 1)
                || (n.y == node.y && n.x == node.x + 1)
            {
                n.dist = n.danger + node.dist;
            };
            n
        }).collect();
}

fn parse_input() -> Vec<Vec<u32>> {
    include_str!("../input.txt")
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn make_dists() -> ((usize, usize), Vec<Node>) {
    let input = enlarge_map(parse_input());

    (
        (input[0].len() - 1, input.len() - 1),
        input
            .into_iter()
            .enumerate()
            .map(|(y, vs)| {
                vs.into_iter().enumerate().map(move |(x, v)| Node {
                    x,
                    y,
                    danger: if x == 0 && y == 0 { 0 } else { v },
                    dist: if x == 0 && y == 0 { 0 } else { u32::MAX },
                })
            })
            .flatten()
            .collect(),
    )
}

fn enlarge_map(input: Vec<Vec<u32>>) -> Vec<Vec<u32>> {
    let enlarged_width: Vec<Vec<u32>> = repeat(input.clone())
        .take(4)
        .enumerate()
        .fold(input, |acc, (ic, m)| {
            acc.into_iter()
                .enumerate()
                .map(|(ir, mut r)| {
                    let mut updated = m[ir].iter().cloned().map(|v| (v + (ic as u32)).rem_euclid(9) + 1).collect();
                    r.append(&mut updated);
                    r
                })
                .collect()
        });

    repeat(enlarged_width.clone()).take(4).enumerate()
        .fold(enlarged_width, |mut acc, (i, m)| {
            let mut updated = m.into_iter().map(|r| r.into_iter().map(|v| (v + (i as u32)).rem_euclid(9) + 1).collect()).collect();
            acc.append(&mut updated);
            acc
        })
}

