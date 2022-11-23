use std::collections::HashMap;

fn main() {
    let (mut polymer, instructions, mut chars) = parse_input();

    for _ in 0..40 {
        calc_changes(&mut polymer, &instructions, &mut chars);
        dbg!(chars.iter()
            .map(|(_, i)| i)
            .sum::<usize>());
    }

    let res = chars.iter()
        .map(|(_, i)| i)
        .max()
        .unwrap()
        - chars
            .iter()
            .map(|(_, i)| i)
            .min()
            .unwrap();
    dbg!(chars);
    dbg!(res);
}

fn parse_input() -> (HashMap<(char, char), usize>, HashMap<(char, char), char>, HashMap<char, usize>) {
    let mut input = include_str!("../input.txt").lines();

    let start_str = input.next().unwrap().to_owned();
    input.next();
    let mutations: HashMap<(char, char), char> = input
        .map(|i| i.split_once(" -> ").unwrap())
        .map(|(a, b)| ((a.chars().next().unwrap(), a.chars().nth(1).unwrap()), b.chars().next().unwrap()))
        .collect();

    let chars = start_str.chars().fold(HashMap::new(), |mut acc, c| {
                if let Some(v) = acc.get_mut(&c) {
                    *v = *v + 1;
                } else {
                    acc.insert(c, 1);
                }
                acc
    });
    let start = start_str
        .char_indices()
        .fold(HashMap::new(), |mut acc, (i, c)| {
            if i + 1 == start_str.len() {
                acc
            } else {
                let part = (c, start_str.chars().nth(i + 1).unwrap());
                if let Some(v) = acc.get_mut(&part) {
                    *v = *v + 1;
                } else {
                    acc.insert(part, 1);
                }
                acc
            }
        });

    return (start, mutations, chars);
}

fn calc_changes(map: &mut HashMap<(char, char), usize>, instructions: &HashMap<(char, char), char>, chars: &mut HashMap<char, usize>) {
    let start = map.clone();

    for (key, amount) in start.into_iter() {
        let insertion = if let Some(inst) = instructions.get(&key) {
            *inst
        } else {
            continue;
        };

        let a = (key.0, insertion);
        let b = (insertion, key.1);

        if let Some(v) = map.get_mut(&a) {
            *v = *v + amount;
        } else {
            map.insert(a, amount);
        }

        if let Some(v) = map.get_mut(&b) {
            *v = *v + amount;
        } else {
            map.insert(b, amount);
        }

        let old = map.get_mut(&key).unwrap();
        *old = *old - amount;

        if let Some(v) = chars.get_mut(&insertion) {
            *v = *v + amount;
        } else {
            chars.insert(insertion, amount);
        }
    }
}

