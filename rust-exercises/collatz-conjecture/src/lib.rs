pub fn collatz(mut n: u64) -> Option<u64> {
    if n == 0 {
        None
    } else if n == 1 {
        Some(0)
    } else if n % 2 == 0 {
        Some(1 + collatz(n / 2).unwrap())
    } else {
        Some(1 + collatz(3 * n + 1).unwrap())
    }
}
