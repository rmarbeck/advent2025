use std::io::{self, Read};
use std::time::Instant;

#[derive(Clone, Copy, Debug)]
struct JunctionBox {
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Clone, Copy, Debug)]
struct Edge {
    i: usize,
    j: usize,
    dist2: i64,
}

impl Edge {
    fn new(i: usize, j: usize, a: &JunctionBox, b: &JunctionBox) -> Self {
        let dx = (a.x - b.x) as i64;
        let dy = (a.y - b.y) as i64;
        let dz = (a.z - b.z) as i64;
        let dist2 = dx * dx + dy * dy + dz * dz;
        Edge { i, j, dist2 }
    }
}

fn parse_boxes(lines: &[String]) -> Vec<JunctionBox> {
    lines
        .iter()
        .filter_map(|s| {
            let parts: Vec<_> = s.trim().split(',').collect();
            if parts.len() != 3 {
                return None;
            }
            Some(JunctionBox {
                x: parts[0].parse().ok()?,
                y: parts[1].parse().ok()?,
                z: parts[2].parse().ok()?,
            })
        })
        .collect()
}

struct UnionFind {
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            parent: (0..n).collect(),
            size: vec![0; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            let root = self.find(self.parent[x]);
            self.parent[x] = root;
        }
        self.parent[x]
    }

    fn ensure_init(&mut self, x: usize) -> bool {
        if self.size[x] == 0 {
            self.size[x] = 1;
            true
        } else {
            false
        }
    }

    fn union(&mut self, a: usize, b: usize) {
        let mut ra = self.find(a);
        let mut rb = self.find(b);
        if ra == rb {
            return;
        }
        if self.size[ra] < self.size[rb] {
            std::mem::swap(&mut ra, &mut rb);
        }
        self.parent[rb] = ra;
        self.size[ra] += self.size[rb];
    }

    fn top3_product(&self) -> i64 {
        let mut comps = Vec::new();
        for i in 0..self.parent.len() {
            if self.parent[i] == i && self.size[i] > 0 {
                comps.push(self.size[i] as i64);
            }
        }
        if comps.is_empty() {
            return 1;
        }
        comps.sort_unstable();
        comps.iter().rev().take(3).product()
    }
}

fn solve(lines: &[String]) -> (i64, i64) {
    let (count_down, limit) = if lines.len() == 20 {
        (10usize, 10usize)
    } else {
        (1000usize, 1000usize)
    };

    let boxes = parse_boxes(lines);
    let n = boxes.len();
    if n < 2 {
        return (0, 0);
    }

    let mut edges = Vec::with_capacity(n * (n - 1) / 2);
    for i in 0..n {
        for j in (i + 1)..n {
            edges.push(Edge::new(i, j, &boxes[i], &boxes[j]));
        }
    }

    edges.sort_unstable_by(|a, b| a.dist2.cmp(&b.dist2));

    let mut uf = UnionFind::new(n);
    let mut visited_count = 0usize;
    let mut edges_processed = 0usize;
    let mut best3 = 0i64;
    let mut best3_done = false;
    let mut last_edge: Option<Edge> = None;

    for e in edges.into_iter() {
        if visited_count >= limit {
            break;
        }

        last_edge = Some(e);

        if uf.ensure_init(e.i) {
            visited_count += 1;
        }
        if uf.ensure_init(e.j) {
            visited_count += 1;
        }

        uf.union(e.i, e.j);
        edges_processed += 1;

        if !best3_done && edges_processed == count_down {
            best3 = uf.top3_product();
            best3_done = true;
        }
    }

    let part1 = if best3_done { best3 } else { 0 };
    let part2 = last_edge.map_or(0, |e| {
        (boxes[e.i].x as i64) * (boxes[e.j].x as i64)
    });

    (part1, part2)
}

fn main() {
    let start_total = Instant::now();

    // I/O
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let lines: Vec<String> = input.lines().map(|s| s.to_string()).collect();

    let start_compute = Instant::now();

    // Calcul
    let (part1, part2) = solve(&lines);

    let compute_time = start_compute.elapsed();
    let total_time = start_total.elapsed();

    println!("{}", part1);
    println!("{}", part2);

    // Affichage des timings
    eprintln!("--- Performance ---");
    eprintln!("Temps hors I/O : {:?}", compute_time);
    eprintln!("Temps total    : {:?}", total_time);
}

