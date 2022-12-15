use rayon::prelude::*;
use regex::Regex;

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coord(i64, i64);

impl Coord {
    fn distance(&self, other: &Self) -> i64 {
        (self.0 - other.0).abs() + (self.1 - other.1).abs()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Sensor {
    loc: Coord,
    dist: i64,
}

impl Sensor {
    fn blocks_range(&self, y: i64) -> Range {
        let y_dist = (self.loc.1 - y).abs();
        if y_dist >= self.dist {
            return Range(0, 0);
        }

        let diff = self.dist - y_dist;
        Range(self.loc.0 - diff, self.loc.0 + diff)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Range(i64, i64);

impl Range {
    fn merge(&self, other: &Range) -> Option<Range> {
        if self.1 + 1 < other.0 || self.0 > other.1 + 1 {
            return None;
        }

        Some(Range(self.0.min(other.0), self.1.max(other.1)))
    }
}

fn parse_input() -> Vec<Sensor> {
    let mut sensors = Vec::new();
    let reg = Regex::new(r"x=(-?\d+), y=(-?\d+)").unwrap();

    for line in INPUT.lines() {
        let captures = reg.captures_iter(line).collect::<Vec<_>>();
        let loc = Coord(
            captures[0][1].parse().unwrap(),
            captures[0][2].parse().unwrap(),
        );
        let beacon = Coord(
            captures[1][1].parse().unwrap(),
            captures[1][2].parse().unwrap(),
        );
        sensors.push(Sensor {
            dist: loc.distance(&beacon),
            loc,
        })
    }

    sensors
}

fn merge_ranges(mut ranges: Vec<Range>) -> Vec<Range> {
    ranges.sort();
    let mut res = vec![ranges[0]];
    let mut i = 0;

    for range in &ranges[1..] {
        if let Some(new_range) = res[i].merge(range) {
            res[i] = new_range;
        } else {
            res.push(*range);
            i += 1;
        }
    }

    res
}

fn main() {
    let total = 4000000;
    let sensors = parse_input();

    (0..=total).into_par_iter().for_each(|y| {
        let blocked = merge_ranges(
            sensors
                .iter()
                .map(|s| s.blocks_range(y))
                .collect::<Vec<_>>(),
        );

        if blocked.len() > 1 {
            let x = blocked[0].1 + 1;
            println!("{},{} -> {}", x, y, x * 4000000 + y);
            std::process::exit(0);
        }
    });
}
