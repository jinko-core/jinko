use crate::error::Error;

pub trait JkReader: JkReaderClone {
    fn read_to_string(&self, path: &str) -> Result<String, Error>;
}

pub trait JkReaderClone {
    fn box_clone(&self) -> Box<dyn JkReader>;
}

impl<T> JkReaderClone for T
where
    T: 'static + JkReader + Clone,
{
    fn box_clone(&self) -> Box<dyn JkReader> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn JkReader> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

#[cfg(feature = "std")]
#[derive(Clone)]
pub struct JkStdReader {}

#[cfg(feature = "std")]
impl JkReader for JkStdReader {
    fn read_to_string(&self, path: &str) -> Result<String, Error> {
        Ok(std::fs::read_to_string(path)?)
    }
}
