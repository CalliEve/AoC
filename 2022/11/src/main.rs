use std::str::FromStr;

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone)]
enum Num {
    Num(i64),
    Old,
}

impl FromStr for Num {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "old" {
            Ok(Num::Old)
        } else if let Ok(i) = s.parse::<i64>() {
            Ok(Num::Num(i))
        } else {
            Err(format!("can't parse {}", s))
        }
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Mul(Num),
    Add(Num),
}

impl Operator {
    fn exec(&self, old: i64) -> i64 {
        match self {
            Operator::Add(x) => {
                old + match x {
                    Num::Old => old,
                    Num::Num(n) => *n,
                }
            }
            Operator::Mul(x) => {
                old * match x {
                    Num::Old => old,
                    Num::Num(n) => *n,
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<i64>,
    operation: Operator,
    test: i64,
    if_true: usize,
    if_false: usize,
    inspected: i32,
}

impl Monkey {
    fn append(&mut self, mut items: Vec<i64>) {
        self.items.append(&mut items);
    }

    fn inspect(&mut self, monkey_count: usize, div: i64) -> Vec<Vec<i64>> {
        let mut thrown = vec![Vec::new(); monkey_count];

        while let Some(item) = self.items.pop() {
            let val = self.operation.exec(item) % div;
            if val % self.test == 0 {
                thrown[self.if_true].push(val);
            } else {
                thrown[self.if_false].push(val);
            }
            self.inspected += 1;
        }

        thrown
    }
}

fn parse() -> Vec<Monkey> {
    let monkey_input = INPUT.split("\n\n");
    let mut monkey_output = Vec::new();

    for monkey_str in monkey_input {
        let mut monkey = monkey_str.split('\n');
        monkey.next();
        let items = monkey
            .next()
            .unwrap()
            .get(18..)
            .unwrap()
            .split(", ")
            .map(|s| s.parse().unwrap())
            .collect::<Vec<i64>>();
        let operation = monkey
            .next()
            .unwrap()
            .get(23..)
            .map(|x| {
                let mut stmt = x.split(' ');
                match stmt.next() {
                    Some("+") => Operator::Add(stmt.next().unwrap().parse().unwrap()),
                    Some("*") => Operator::Mul(stmt.next().unwrap().parse().unwrap()),
                    _ => panic!("can't parse {}", x),
                }
            })
            .unwrap();
        let test = monkey
            .next()
            .unwrap()
            .get(21..)
            .unwrap()
            .parse::<i64>()
            .unwrap();
        let if_true = monkey
            .next()
            .unwrap()
            .get(29..)
            .unwrap()
            .parse::<usize>()
            .unwrap();
        let if_false = monkey
            .next()
            .unwrap()
            .get(30..)
            .unwrap()
            .parse::<usize>()
            .unwrap();
        monkey_output.push(Monkey {
            items,
            operation,
            test,
            if_true,
            if_false,
            inspected: 0,
        })
    }
    monkey_output
}

fn round(monkeys: &mut Vec<Monkey>, div: i64) {
    let monkey_count = monkeys.len();

    for i in 0..monkeys.len() {
        let thrown = monkeys[i].inspect(monkey_count, div);

        for (j, items) in thrown.into_iter().enumerate() {
            monkeys[j].append(items);
        }
    }
}

fn main() {
    let mut monkeys = parse();
    let div = monkeys.iter().map(|m| m.test).fold(1, |acc, x| acc * x);

    for _ in 0..10000 {
        round(&mut monkeys, div);
    }

    let res = monkeys
        .iter()
        .map(|m| i64::from(m.inspected))
        .fold([0_i64, 0], |mut acc, x| {
            if acc[1] < x {
                acc[1] = x;
            }
            if acc[0] < acc[1] {
                let tmp = acc[0];
                acc[0] = acc[1];
                acc[1] = tmp;
            }
            acc
        });

    println!("{}", res[0] * res[1])
}
