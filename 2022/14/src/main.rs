const INPUT: &str = include_str!("../input.txt");

type Board = Vec<Vec<char>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Coord(usize, usize);

impl Coord {
    fn range(&self, stop: Coord) -> Vec<Coord> {
        let mut res = Vec::new();

        if self.0 < stop.0 {
            for x in self.0..=stop.0 {
                res.push(Coord(x, self.1));
            }
        } else if self.0 > stop.0 {
            for x in stop.0..=self.0 {
                res.push(Coord(x, self.1));
            }
        } else if self.1 < stop.1 {
            for y in self.1..=stop.1 {
                res.push(Coord(self.0, y));
            }
        } else if self.1 > stop.1 {
            for y in stop.1..=self.1 {
                res.push(Coord(self.0, y));
            }
        } else {
            res.push(*self);
        }

        res
    }
}

fn paint_board_line(board: &mut Board, input: Vec<Coord>) {
    for window in input.windows(2) {
        for point in window[0].range(window[1]) {
            board[point.1][point.0] = '#'
        }
    }
}

fn parse_input() -> Board {
    let mut max_y = 0;

    let input = INPUT
        .lines()
        .map(|x| {
            x.split(" -> ")
                .map(|s| {
                    let raw: Vec<usize> = s.split(",").map(|i| i.parse().unwrap()).collect();
                    if raw[1] > max_y {
                        max_y = raw[1]
                    }

                    Coord(raw[0], raw[1])
                })
                .collect::<Vec<Coord>>()
        })
        .collect::<Vec<Vec<Coord>>>();

    let mut board = vec![Vec::new(); 200];
    board.fill_with(|| vec!['.'; 700]);

    for line in input {
        paint_board_line(&mut board, line);
    }

    paint_board_line(&mut board, vec![Coord(0, max_y + 2), Coord(699, max_y + 2)]);

    board
}

fn drop_sand_block(board: &mut Board, block: &mut Coord) -> bool {
    if board[block.1 + 1][block.0] == '.' {
        *block = Coord(block.0, block.1 + 1);
        true
    } else if board[block.1 + 1][block.0 - 1] == '.' {
        *block = Coord(block.0 - 1, block.1 + 1);
        true
    } else if board[block.1 + 1][block.0 + 1] == '.' {
        *block = Coord(block.0 + 1, block.1 + 1);
        true
    } else {
        false
    }
}

fn drop_sand(board: &mut Board) -> bool {
    let mut sand_loc = Coord(500, 0);

    while drop_sand_block(board, &mut sand_loc) {}

    if board[sand_loc.1][sand_loc.0] == 'o' {
        return false;
    }

    board[sand_loc.1][sand_loc.0] = 'o';

    true
}

fn main() {
    let mut board = parse_input();

    let mut drops = 0;

    while drop_sand(&mut board) {
        drops += 1;
    }

    println!("{}", drops);
}
