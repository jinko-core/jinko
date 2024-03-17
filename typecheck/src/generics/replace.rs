use fir::iter::Mapper;
use flatten::FlattenData;

pub struct Replace;

// FIXME: Can there be errors?
pub struct Error;

// FIXME: Can we have a better type here for `U`? Or should that be part of a later rewrite where we get rid of FlattenData
// this far in the pipeline anyway?
impl<'ast> Mapper<FlattenData<'ast>, FlattenData<'ast>, Error> for Replace {}
