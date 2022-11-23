use std::collections::HashMap;

const INPUT: &str = include_str!("../input.txt");

fn main() {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    parse_input_to_graph(&mut graph);
    dbg!(&graph);

    let mut st = Vec::new();
    st.push("start".to_owned());

    let mut paths: Vec<Vec<String>> = Vec::new();

    while let Some(p) = visit_node("start", &mut st, &mut paths, &mut graph, false) {
        paths.push(p);

        st.clear();
        st.push("start".to_owned());
    }

    dbg!(&paths);
    dbg!(paths.len());
}

fn parse_input_to_graph(graph: &mut HashMap<String, Vec<String>>) {
    INPUT.lines().for_each(|l| add_path(l, graph));
}

fn add_path(path: &str, graph: &mut HashMap<String, Vec<String>>) {
    match path.split_once('-') {
        Some((a, b)) => {
            if let Some(n) = graph.get_mut(a) {
                n.push(b.to_owned());
            } else {
                graph.insert(a.to_owned(), vec![b.to_owned()]);
            }
            if let Some(n) = graph.get_mut(b) {
                n.push(a.to_owned());
            } else {
                graph.insert(b.to_owned(), vec![a.to_owned()]);
            }
        }
        None => panic!("Invalid path"),
    }
}

fn visit_node(
    node: &str,
    st: &mut Vec<String>,
    all: &mut Vec<Vec<String>>,
    graph: &mut HashMap<String, Vec<String>>,
    small_twice: bool,
) -> Option<Vec<String>> {
    if node == "end" {
        if all.contains(st) {
            return None;
        } else {
            return Some(st.clone());
        }
    }

    let children = graph.get(node).unwrap().clone();

    for child in children {
        if child == "start"
            || (child.chars().all(char::is_lowercase)
                && st.iter().filter(|v| *v == &child).count() >= 2)
        {
            continue;
        }

        if st.contains(&child) && child.chars().all(char::is_lowercase) {
            if !small_twice {
                st.push(child.clone());
                if let Some(v) = visit_node(&child, st, all, graph, true) {
                    all.push(v);
                    dbg!(all.len());
                }
                st.pop();
            }
            continue;
        }

        st.push(child.clone());
        if let Some(v) = visit_node(&child, st, all, graph, small_twice) {
            all.push(v);
            dbg!(all.len());
        }
        st.pop();
    }

    return None;
}
