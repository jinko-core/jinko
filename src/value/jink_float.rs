//! Represents a floating point number in Jinko. Floating point numbers are always
//! double precision

pub struct JinkFloat(f64);

impl From<f64> for JinkFloat {
    fn from(f: f64) -> Self {
        JinkFloat(f)
    }
}
