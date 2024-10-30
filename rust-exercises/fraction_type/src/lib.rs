use std::ops;

#[derive(Debug, PartialEq, Eq)]
pub struct Fraction(pub i32, pub i32);

impl Fraction {
    pub fn add(&self, rhs: Fraction) -> Fraction {
        simplify(self.0 * rhs.1 + rhs.0 * self.1, self.1 * rhs.1)
    }

    pub fn sub(&self, rhs: Fraction) -> Fraction {
        simplify(self.0 * rhs.1 - rhs.0 * self.1, self.1 * rhs.1)
    }

    pub fn mul(&self, rhs: Fraction) -> Fraction {
        simplify(self.0 * rhs.0, self.1 * rhs.1)
    }

    pub fn divide(&self, rhs: Fraction) -> Fraction {
        simplify(self.0 * rhs.1, self.1 * rhs.0)
    }
}

impl ops::Add for Fraction {
    type Output = Fraction;

    fn add(self, rhs: Self) -> Self::Output {
        simplify(self.0 * rhs.1 + rhs.0 * self.1, self.1 * rhs.1)
    }
}

/// Calculate the Highest common factor between 2 numbers
fn hcf(a: i32, b: i32) -> i32 {
    if b == 0 {
        a
    } else {
        hcf(b, a % b)
    }
}

fn simplify(n: i32, d: i32) -> Fraction {
    let h = hcf(n, d);
    Fraction(n / h, d / h)
}
