pub fn diagonal(n: u32, p: u32) -> u64 {
    let mut diag = Vec::from_iter(std::iter::repeat(1u64).take((n + 1) as usize));
    for i in 1..=p {
        for j in 1..n + 1 - i {
            diag[j as usize] += diag[(j - 1) as usize];
        }
    }
    diag[0..(n + 1 - p) as usize].iter().sum()
}
