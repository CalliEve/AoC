use std::collections::{HashSet, VecDeque};

const INPUT: &str = include_str!("../input.txt");

const ORIENTATIONS: [[[i32; 3]; 3]; 24] = [
    [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
    [[1, 0, 0], [0, 0, -1], [0, 1, 0]],
    [[1, 0, 0], [0, -1, 0], [0, 0, -1]],
    [[1, 0, 0], [0, 0, 1], [0, -1, 0]],
    [[0, -1, 0], [1, 0, 0], [0, 0, 1]],
    [[0, 0, 1], [1, 0, 0], [0, 1, 0]],
    [[0, 1, 0], [1, 0, 0], [0, 0, -1]],
    [[0, 0, -1], [1, 0, 0], [0, -1, 0]],
    [[-1, 0, 0], [0, -1, 0], [0, 0, 1]],
    [[-1, 0, 0], [0, 0, -1], [0, -1, 0]],
    [[-1, 0, 0], [0, 1, 0], [0, 0, -1]],
    [[-1, 0, 0], [0, 0, 1], [0, 1, 0]],
    [[0, 1, 0], [-1, 0, 0], [0, 0, 1]],
    [[0, 0, 1], [-1, 0, 0], [0, -1, 0]],
    [[0, -1, 0], [-1, 0, 0], [0, 0, -1]],
    [[0, 0, -1], [-1, 0, 0], [0, 1, 0]],
    [[0, 0, -1], [0, 1, 0], [1, 0, 0]],
    [[0, 1, 0], [0, 0, 1], [1, 0, 0]],
    [[0, 0, 1], [0, -1, 0], [1, 0, 0]],
    [[0, -1, 0], [0, 0, -1], [1, 0, 0]],
    [[0, 0, -1], [0, -1, 0], [-1, 0, 0]],
    [[0, -1, 0], [0, 0, 1], [-1, 0, 0]],
    [[0, 0, 1], [0, 1, 0], [-1, 0, 0]],
    [[0, 1, 0], [0, 0, -1], [-1, 0, 0]],
];

fn main() {
    let mut input = parse_scanners();
    let mut base: HashSet<Vec<i32>> = input.remove(0).into_iter().collect();
    let mut to_find: VecDeque<Vec<Vec<i32>>> = input.into_iter().collect();
    let mut scanners = Vec::new();

    'scanner: while let Some(scanner) = to_find.pop_front() {
        for orientation in ORIENTATIONS {
            let mut found = Vec::new();

            for beacon in &scanner {
                let turned = change_orientation(&beacon, orientation);
                let mut found_b = Vec::new();

                for b_beacon in &base {
                    found_b.push(calc_diff(&b_beacon, &turned));
                }

                found.push(found_b);
            }

            let (count, diff): (usize, Vec<i32>) = calc_in_common(found);

            if count >= 12 {
                for beacon in &scanner {
                    let turned = change_orientation(&beacon, orientation);
                    base.insert(vec![
                        turned[0] + diff[0],
                        turned[1] + diff[1],
                        turned[2] + diff[2],
                    ]);
                }
                scanners.push(diff);
                continue 'scanner;
            }
        }
        to_find.push_back(scanner);
    }

    println!("amount of beacons: {}", base.len());

    println!("max distance: {}", find_max_distance(scanners));
}

fn parse_scanners() -> Vec<Vec<Vec<i32>>> {
    let mut res = Vec::new();

    let mut beacons = Vec::new();
    for line in INPUT.lines() {
        if line.starts_with("---") {
            continue;
        } else if line.is_empty() {
            res.push(beacons.clone());
            beacons.clear();
        } else {
            beacons.push(line.split(',').map(|v| v.parse().unwrap()).collect());
        }
    }
    res.push(beacons);

    res
}

fn change_orientation(input: &[i32], orientation: [[i32; 3]; 3]) -> Vec<i32> {
    vec![
        orientation[0][0] * input[0] + orientation[0][1] * input[1] + orientation[0][2] * input[2],
        orientation[1][0] * input[0] + orientation[1][1] * input[1] + orientation[1][2] * input[2],
        orientation[2][0] * input[0] + orientation[2][1] * input[1] + orientation[2][2] * input[2],
    ]
}

fn calc_diff(a: &[i32], b: &[i32]) -> Vec<i32> {
    vec![a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn calc_in_common(all: Vec<Vec<Vec<i32>>>) -> (usize, Vec<i32>) {
    let mut max_found = (0, Vec::new());

    for (i, beacon) in all.iter().enumerate() {
        for diff in beacon {
            let mut count = 1;

            for other_beacon in &all[(i + 1)..] {
                if other_beacon.contains(diff) {
                    count += 1;
                }
            }

            if count > max_found.0 {
                max_found = (count, diff.clone());
            }
        }
    }

    max_found
}

fn find_max_distance(scanners: Vec<Vec<i32>>) -> i32 {
    scanners
        .iter()
        .enumerate()
        .map(|(i, scanner)| {
            scanners[(i + 1)..]
                .iter()
                .map(|s| {
                    (scanner[0] - s[0]).abs()
                        + (scanner[1] - s[1]).abs()
                        + (scanner[2] - s[2]).abs()
                })
                .max()
                .unwrap_or(0)
        })
        .max()
        .unwrap()
}
