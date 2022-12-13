use serde::{Deserialize};
use std::cmp::Ordering;
use std::fmt;

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
enum LineContent {
    Num(i32),
    List(Vec<Box<LineContent>>),
}

impl fmt::Display for LineContent {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LineContent::Num(n) => write!(fmt, "{}", n),
            LineContent::List(l) => write!(fmt, "[{}]", l.iter().map(|x| x.to_string() + ",").collect::<String>().trim_matches(','))
        }
    }
}

impl LineContent {
    fn cmp_with_num(&self, other: i32) -> Option<Ordering> {
        match self {
            LineContent::Num(n) => n.partial_cmp(&other),
            LineContent::List(_) => {
                self.cmp_with_list(&vec![Box::new(LineContent::Num(other))])
            }
        }
    }

    fn cmp_with_list(&self, other: &Vec<Box<LineContent>>) -> Option<Ordering> {
        match self {
                LineContent::Num(_) => {
                    LineContent::List(vec![Box::new(self.clone())]).cmp_with_list(other)
                },
                LineContent::List(l) => {
                    for (i, c) in l.iter().enumerate() {
                        if i >= other.len() {
                            return Some(Ordering::Greater)
                        }

                        let cmp = c.partial_cmp(&other[i]);

                        if cmp == Some(Ordering::Equal)  {
                            continue;
                        }

                        return cmp
                    }

                    if l.len() == other.len() {
                        return Some(Ordering::Equal)
                    }
                    
                    return Some(Ordering::Less)
                }
        }
    }
}

impl PartialOrd for LineContent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match other {
            LineContent::Num(n) => self.cmp_with_num(*n),
            LineContent::List(l) => self.cmp_with_list(l),
        }
    }
}

impl Ord for LineContent {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn main() {
    let dividers = "\n[[2]]\n[[6]]";
    let input = String::from(INPUT) + dividers;

    let mut c = input
        .replace("\n\n", "\n")
        .trim()
        .lines()
        .map(|x| serde_json::from_str(x).unwrap())
        .collect::<Vec<LineContent>>();
    
    c.sort();
    
    let div_one = c.iter().position(|x| x == &LineContent::List(vec![Box::new(LineContent::List(vec![Box::new(LineContent::Num(2))]))])).unwrap();
    let div_two = c.iter().position(|x| x == &LineContent::List(vec![Box::new(LineContent::List(vec![Box::new(LineContent::Num(6))]))])).unwrap();

    println!("{}", c.iter().map(|x| x.to_string() + "\n").collect::<String>());
    println!("{} * {} = {}", (div_one + 1), (div_two + 1), (div_one + 1) * (div_two + 1))
}
