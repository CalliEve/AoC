use std::collections::{HashMap, VecDeque};

const INPUT: &str = include_str!("../input.txt");

#[derive(Debug, Clone)]
enum Tree {
    Dir {
        name: String,
        value: i32,
        files: HashMap<String, Box<Tree>>,
    },
    File {
        name: String,
        value: i32,
    },
}

impl Tree {
    fn get_files(&self) -> Result<&HashMap<String, Box<Tree>>, String> {
        match self {
            Tree::Dir { files, .. } => Ok(files),
            _ => Err("no".to_owned()),
        }
    }

    fn insert(&mut self, name: String, dir: Box<Tree>) -> Result<(), String> {
        match self {
            Tree::Dir { files, .. } => {
                files.insert(name, dir);
                Ok(())
            }
            _ => Err("no".to_owned()),
        }
    }

    fn get_name(&self) -> String {
        match self {
            Tree::Dir { name, .. } => name.clone(),
            Tree::File { name, .. } => name.clone(),
        }
    }

    fn update_value(&mut self) -> i32 {
        match self {
            Tree::Dir { files, value, .. } => {
                let mut total: i32 = 0;
                for file in files.iter_mut() {
                    total += file.1.update_value();
                }

                *value = total;
                total
            }
            Tree::File { value, .. } => *value,
        }
    }

    fn get_value(&self) -> i32 {
        match self {
            Tree::Dir { value, .. } => *value,
            Tree::File { value, .. } => *value,
        }
    }
}

fn parse(input: Vec<&str>) -> Result<Box<Tree>, String> {
    let root = Tree::Dir {
        name: "/".to_owned(),
        value: 0,
        files: HashMap::new(),
    };
    let mut past = vec![Box::new(root)];

    for line in input {
        if line.starts_with("$ ls") {
            continue;
        }
        if line.starts_with("$ cd /") {
            while past.len() > 1 {
                let d = past.pop().unwrap();
                past.last_mut().unwrap().insert(d.get_name(), d).unwrap();
            }
            continue;
        }
        if line.starts_with("$ cd ..") {
            let d = past.pop().unwrap();
            past.last_mut().unwrap().insert(d.get_name(), d).unwrap();
            continue;
        }
        if line.starts_with("$ cd ") {
            let name = &line[5..];
            let dir = past
                .last()
                .unwrap()
                .get_files()
                .unwrap()
                .get(&name.to_owned())
                .unwrap()
                .clone();
            past.push(dir);
            continue;
        }
        if line.starts_with("dir ") {
            let name = &line[4..];
            let d = Box::new(Tree::Dir {
                files: HashMap::new(),
                name: name.to_owned(),
                value: 0,
            });
            past.last_mut().unwrap().insert(d.get_name(), d).unwrap();
            continue;
        }

        let splitted = line.split(' ').collect::<Vec<_>>();
        let name = splitted[1].to_owned();
        let value = splitted[0].parse::<i32>().unwrap();
        let d = Box::new(Tree::File { name, value });
        past.last_mut().unwrap().insert(d.get_name(), d).unwrap();
    }

    while past.len() > 1 {
        let d = past.pop().unwrap();
        past.last_mut().unwrap().insert(d.get_name(), d).unwrap();
    }

    let mut root = past.pop().unwrap();
    root.update_value();
    Ok(root)
}

fn get_total(tree: Box<Tree>) -> i32 {
    let minimum = 30000000 - (70000000 - tree.get_value());

    let mut smallest = tree.get_value();
    let mut q = VecDeque::new();
    q.push_back(&tree);

    while let Some(node) = q.pop_front() {
        if let Ok(subfiles) = node.get_files() {
            for f in subfiles.iter() {
                q.push_back(f.1);
            }

            let val = node.get_value();
            if val > minimum && val < smallest {
                smallest = val;
            }
        }
    }

    smallest
}

fn main() {
    let lines: Vec<&str> = INPUT.split("\n").collect();

    let tree = parse(lines).unwrap();

    println!("{}", get_total(tree));
}
