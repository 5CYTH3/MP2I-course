/// * `k` - le nombre magique
/// * `n` - le nombre de personnes
/// * `tailles` - la liste des tailles de chaque personne
fn ordre(k: i32, n: i32, mut tailles: Vec<i32>) {
    /* TODO Afficher **OUI** s'il est possible de trier les personnes par
    taille ou **NON** si ce n'est pas possible. */

    let mut i: usize = 0;
    while i < n as usize {
        if tailles.get(i + k as usize).is_none() {
            break;
        }

        if tailles.get(i).unwrap() > tailles.get(i + k as usize).unwrap() {
            tailles.swap(i, i + k as usize);
            i = 0;
        } else {
            i += 1
        }
    }

    fn is_sorted(vec: &Vec<i32>) -> bool {
        for i in 0..vec.len() - 2 {
            if vec[i] > vec[i + 1] {
                return false;
            }
        }
        true
    }

    if is_sorted(&tailles) {
        println!("{:?}", tailles);
        println!("OUI");
    } else {
        println!("{:?}", tailles);
        println!("NON")
    }
}

fn main() {
    /*
    let mut buffer = String::new();
    let k = read_line(&mut buffer)
        .parse()
        .expect("invalid `K` parameter");

    let n = read_line(&mut buffer)
        .parse()
        .expect("invalid `N` parameter");

    let tailles = read_line(&mut buffer)
        .split_whitespace()
        .map(str::parse)
        .collect::<Result<_, _>>()
        .expect("invalid `tailles` parameter");
    */
    ordre(2, 5, vec![5, 4, 3, 2, 1]);
}

fn read_line(buffer: &mut String) -> &str {
    buffer.clear();
    std::io::stdin()
        .read_line(buffer)
        .expect("impossible to read a new line");
    buffer.trim_end()
}
