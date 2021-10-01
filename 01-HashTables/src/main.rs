use std::collections::HashSet;
use std::hash::{Hash, Hasher};

#[macro_use]
extern crate dmoj;

#[derive(Debug)]
pub struct Snowflake([u32; 6]);

impl PartialEq for Snowflake {
    fn eq(&self, other: &Self) -> bool {
        for i in 0..6 {
            if (0..6).all(|j| self.0[j] == other.0[(i + j) % 6]) ||
               ((0..6).rev().all(|j| self.0[j] == other.0[(i + (5 - j)) % 6]))
               {return true}
        }
        false
    }
}

impl Eq for Snowflake {}

impl Hash for Snowflake {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.iter().sum::<u32>().hash(state)
    }
}

fn snowflake() -> Snowflake {
    let mut a = [0;6];
    for i in 0..6 {a[i] = scan!(u32)}
    Snowflake(a)
}

fn main() {
    let mut table: HashSet<Snowflake> = HashSet::new();
    let mut found = false;
    let n = scan!(u32);
    for _ in 0..n {
        let sf = snowflake();
        if !table.insert(sf) {
            println!("Twin snowflakes found.");
            found = true;
            break
        }
    }
    if !found {println!("No two snowflakes are alike.")}
}