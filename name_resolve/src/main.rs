use fir::Fir;
use name_resolve::NameResolve;

fn main() {
    let fir = Fir::default();
    let _fir = fir.name_resolve();
}
