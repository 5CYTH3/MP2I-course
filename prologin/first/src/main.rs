fn main() {
    let (n, differences) = parse("6\n1 6 -7 9 10 -15".to_string());
    le_plus_grand_saut(n, differences);
}

fn le_plus_grand_saut(n: i32, differences: Vec<i32>) {
    let branches: Vec<i32> = differences
        .iter()
        .scan(0, |sum, i| {
            *sum += i;
            Some(*sum)
        })
        .collect();

    let max = branches.iter().max().unwrap();
    let id_max = branches.iter().position(|r| r == max).unwrap();
    let highest = differences.iter().take(id_max + 1).max().unwrap();
    println!("{}", highest)
}

fn parse(input: String) -> (i32, Vec<i32>) {
    let lines: Vec<&str> = input.split("\n").collect();
    let first = lines[0].parse::<i32>().unwrap();
    let last = lines[1]
        .split(" ")
        .collect::<Vec<&str>>()
        .iter()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();
    (first, last)
}
