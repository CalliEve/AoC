use std::fmt::Display;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Location {
    Hallway(usize),
    End(usize, usize),
}

impl Location {
    fn get_content(&self, board: &Board) -> char {
        match self {
            Location::Hallway(l) => board.hallway[*l],
            Location::End(x, y) => board.ends[*x][*y],
        }
    }

    fn is_hallway(&self) -> bool {
        matches!(self, Location::Hallway(_))
    }

    fn is_valid_start(&self, b: &Board, c: char) -> bool {
        match self {
            Location::Hallway(_) => true,
            Location::End(x, y) => {
                if match c {
                    'A' => *x == 0,
                    'B' => *x == 1,
                    'C' => *x == 2,
                    'D' => *x == 3,
                    _ => unreachable!(),
                } {
                    b.ends[*x][*y..=3].iter().any(|v| *v != c)
                } else {
                    true
                }
            }
        }
    }

    fn is_valid_end(&self, b: &Board, c: char) -> bool {
        match self {
            Location::Hallway(l) => ![2, 4, 6, 8].contains(l),
            Location::End(x, y) => {
                if match c {
                    'A' => *x == 0,
                    'B' => *x == 1,
                    'C' => *x == 2,

                    'D' => *x == 3,
                    _ => unreachable!(),
                } {
                    *y == 3 || b.ends[*x][(y + 1)..=3].iter().all(|v| *v == c)
                } else {
                    false
                }
            }
        }
    }

    fn equal_end_row(&self, other: &Location) -> bool {
        match self {
            Location::Hallway(_) => other.is_hallway(),
            Location::End(x1, _) => match other {
                Location::Hallway(_) => false,
                Location::End(x2, _) => x1 == x2,
            },
        }
    }

    fn get_hallway_pos(&self) -> usize {
        match self {
            Location::Hallway(l) => *l,
            Location::End(x, _) => 2 + (x * 2),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Move {
    from: Location,
    to: Location,
}

impl Move {
    fn get_all_locations(&self) -> Vec<Location> {
        let mut result = Vec::new();

        if let Location::End(x, y) = self.from {
            for i in (0..=y).into_iter().rev() {
                result.push(Location::End(x, i));
            }
        }

        let hs = self.to.get_hallway_pos().min(self.from.get_hallway_pos());
        let he = self.to.get_hallway_pos().max(self.from.get_hallway_pos());

        for i in hs..=he {
            result.push(Location::Hallway(i))
        }

        if self.from.is_hallway() && self.from.get_hallway_pos() == he {
            result.reverse()
        }

        if let Location::End(x, y) = self.to {
            for i in 0..=y {
                result.push(Location::End(x, i));
            }
        }

        result
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Hash)]
struct Board {
    ends: [[char; 4]; 4],
    hallway: [char; 11],
    points: usize,
}

impl Board {
    fn new() -> Self {
        Self {
            ends: [
                ['C', 'D', 'D', 'C'],
                ['A', 'C', 'B', 'A'],
                ['B', 'B', 'A', 'D'],
                ['D', 'A', 'C', 'B'],
            ],
            hallway: ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
            points: 0,
        }
    }

    fn finished(&self) -> bool {
        self.ends[0].iter().all(|v| *v == 'A')
            && self.ends[1].iter().all(|v| *v == 'B')
            && self.ends[2].iter().all(|v| *v == 'C')
            && self.ends[3].iter().all(|v| *v == 'D')
    }

    fn set_location(&mut self, location: Location, c: char) {
        match location {
            Location::Hallway(x) => self.hallway[x] = c,
            Location::End(x, y) => self.ends[x][y] = c,
        }
    }

    fn is_valid_move(&self, m: Move) -> Result<(char, usize), String> {
        let c = m.from.get_content(self);

        if !m.to.is_valid_end(self, c) {
            return Err("wrong end for char".to_owned());
        }

        if m.to.equal_end_row(&m.from) {
            return Err("can only move twice".to_owned());
        }
        
        if !m.from.is_valid_start(self, c) {
            return Err("no exiting filled end".to_owned());
        }

        let all_locs = m.get_all_locations();
        if all_locs[1..].iter().any(|l| l.get_content(self) != '.') {
            return Err(format!("through already occupied location: {:?}", all_locs[1..].iter().map(|l| (l, l.get_content(self))).collect::<Vec<_>>()));
        }

        Ok((c, get_costs(c, all_locs.len() - 1)))
    }

    fn move_char(&mut self, m: Move) -> Result<(), String> {
        let mres = self.is_valid_move(m).map_err(|e|e.to_string())?;

        self.set_location(m.from, '.');
        self.set_location(m.to, mres.0);

        self.points += mres.1;

        Ok(())
    }

    fn get_all_locations(&self) -> (Vec<Location>, Vec<Location>) {
        let mut empty = Vec::new();
        let mut filled = Vec::new();

        for (i, c) in self.hallway.iter().enumerate() {
            if *c == '.' {
                empty.push(Location::Hallway(i));
            } else {
                filled.push(Location::Hallway(i));
            }
        }

        for (x, e) in self.ends.iter().enumerate() {
            for (y, c) in e.iter().enumerate() {
                if *c == '.' {
                    empty.push(Location::End(x, y));
                } else {
                    filled.push(Location::End(x, y));
                }
            }
        }

        (empty, filled)
    }

    fn get_all_possible_moves(&self) -> Vec<(usize, Move)> {
        let mut res = Vec::new();
        let (empty, filled) = self.get_all_locations();

        for empty_loc in empty {
            for filled_loc in filled.iter() {
                let m = Move {
                    from: *filled_loc,
                    to: empty_loc,
                };
                if let Ok((_, cost)) = self.is_valid_move(m) {
                    res.push((cost, m));
                }
            }
        }

        res.sort_by(|m, o| m.0.cmp(&o.0));
        res
    }
}

impl Display for Board {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str(&format!("#############\n#{}#\n###{}#{}#{}#{}###\n  #{}#{}#{}#{}#\n  #{}#{}#{}#{}#\n  #{}#{}#{}#{}#\n  #########\n{}",
            self.hallway.iter().fold("".to_owned(), |acc, x| format!("{}{}", acc, x)),
            self.ends[0][0], self.ends[1][0], self.ends[2][0], self.ends[3][0],
            self.ends[0][1], self.ends[1][1], self.ends[2][1], self.ends[3][1],
            self.ends[0][2], self.ends[1][2], self.ends[2][2], self.ends[3][2],
            self.ends[0][3], self.ends[1][3], self.ends[2][3], self.ends[3][3],
            self.points
            )
        )
    }
}

fn get_costs(c: char, l: usize) -> usize {
    match c {
        'A' => l,
        'B' => 10 * l,
        'C' => 100 * l,
        'D' => 1000 * l,
        _ => unreachable!(),
    }
}

lazy_static!(
    pub static ref RESULTS: Arc<Mutex<usize>> = Arc::new(Mutex::new(usize::MAX));
);

#[memoize::memoize]
fn get_path(b: Board, m: Move) {
    let mut b = b;
    b.move_char(m).unwrap();

    if b.finished() {
        let mut best = RESULTS.lock().unwrap();
        if b.points < *best {
            *best = b.points;
        } else {
            return;
        }

        println!("{}", b)
    }

    b.get_all_possible_moves().par_iter().for_each(|(_,nm)|{
        get_path(b, *nm);
    });
}

fn main() {
    let b = Board::new();

    println!("{}", b);

    b.get_all_possible_moves().par_iter().for_each(|(_,m)|{
        get_path(b, *m)
    });

    println!("solution: {}", RESULTS.lock().unwrap());
}

