use std::collections::BinaryHeap;

const INPUT: &str = include_str!("../input.txt");

const ORDER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coord(usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Square(char, i32, Coord, bool);

impl PartialOrd for Square {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.1 < other.1 {
            Some(std::cmp::Ordering::Greater)
        } else if self.1 == other.1 {
            if self.0 > other.0 {
                Some(std::cmp::Ordering::Greater)
            } else {
                Some(std::cmp::Ordering::Less)
            }
        } else {
            Some(std::cmp::Ordering::Less)
        }
    }
}

impl Ord for Square {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.1 < other.1 {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Less
        }
    }
}

fn parse() -> (Vec<Vec<Square>>, Square) {
    let mut grid: Vec<Vec<Square>> = INPUT
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| Square(c, i32::MAX, Coord(0, 0), false))
                .collect()
        })
        .collect();

    let mut start = grid[0][0];
    for (y, line) in grid.iter_mut().enumerate() {
        for (x, square) in line.iter_mut().enumerate() {
            square.2 = Coord(y, x);

            if square.0 == 'E' {
                start = *square;
            }
        }
    }

    (grid, start)
}

fn get_coord(grid: &Vec<Vec<Square>>, pos: Coord) -> (Square, i32) {
    let found = grid[pos.0][pos.1];
    (
        found,
        ORDER
            .iter()
            .position(|c| *c == found.0)
            .map(|p| p as i32)
            .unwrap_or_else(|| if found.0 == 'E' { 25 } else { -1 }),
    )
}

fn set_coord(grid: &mut Vec<Vec<Square>>, pos: Coord, dist: i32) {
    grid[pos.0][pos.1].1 = dist;
}

fn visit_coord(grid: &mut Vec<Vec<Square>>, pos: Coord) {
    grid[pos.0][pos.1].3 = true;
}

fn get_options(grid: &Vec<Vec<Square>>, pos: Coord) -> Vec<Coord> {
    let (_, val) = get_coord(grid, pos);

    let mut moves = Vec::new();
    if pos.1 < grid[0].len() - 1 {
        let right = Coord(pos.0, pos.1 + 1);
        let (_, right_val) = get_coord(grid, right);
        if right_val >= val - 1 {
            moves.push(right);
        }
    }

    if pos.1 > 0 {
        let left = Coord(pos.0, pos.1 - 1);
        let (_, left_val) = get_coord(grid, left);
        if left_val >= val - 1 {
            moves.push(left);
        }
    }

    if pos.0 < grid.len() - 1 {
        let bottom = Coord(pos.0 + 1, pos.1);
        let (_, bottom_val) = get_coord(grid, bottom);
        if bottom_val >= val - 1 {
            moves.push(bottom);
        }
    }

    if pos.0 > 0 {
        let top = Coord(pos.0 - 1, pos.1);
        let (_, top_val) = get_coord(grid, top);
        if top_val >= val - 1 {
            moves.push(top);
        }
    }

    moves
}

fn fill_in(grid: &mut Vec<Vec<Square>>, square: Square) -> Result<Vec<Square>, i32> {
    if square.0 == 'a' {
        return Err(square.1);
    }

    let mut found = Vec::new();
    for opt in get_options(grid, square.2) {
        let (mut opt_square, _) = get_coord(grid, opt);

        if opt_square.1 > square.1 {
            set_coord(grid, opt, square.1 + 1);
            opt_square.1 = square.1 + 1;
            if !opt_square.3 {
                opt_square.3 = true;
                visit_coord(grid, opt_square.2);
                found.push(opt_square)
            }
        }
    }

    Ok(found)
}

fn main() {
    let (mut grid, mut me) = parse();
    set_coord(&mut grid, me.2, 0);
    visit_coord(&mut grid, me.2);
    me.1 = 0;

    let mut q = BinaryHeap::new();
    q.push(me);

    while let Some(coord) = q.pop() {
        let found_new = fill_in(&mut grid, coord);

        if let Ok(found) = found_new {
            for f in found {
                q.push(f);
            }
        } else {
            println!("{}", found_new.unwrap_err());
            break;
        }
    }
}
