// TODO: Typer could also be responsible for building the list of monomorphization requests (and constraints?)
// so something like a constraints: Map<TypeToMono, Constraints>?
//      -> this is good but it needs to be per invocation - so per function call/type instantiation
//      -> so is it more like a Map<GenericCall, Constraints>?
//      -> Then Checker will be responsible for checking that T: Constraints?
//      -> this seems annoying to deal with
// a good API would be something like - ConstraintBuilder -> Checker -> Monormorphizer
// with Checker checking the constraints? does that make sense?
// do we want to go with an easier first step where we have ConstraintBuilder -> Monomorphizer -> Checker? And we
// don't need to add anything to Checker? Since all functions and types will already be mono'd. But then the errors
// will be shit so we might as well build the constraints from the get-go.
// Now do we want to instead have a ConstraintChecker? who just takes care of checking constraints? then Mono and we
// can probably pass all of that to checker anyway. This is probably a better split. In that case, we should move
// this over to a new module.

// ConstraintBuilder -> ConstraintMap
// ConstraintChecker(ConstraintMap) -> Result<MonoRequests>
// Monomorphizer(MonoRequests) -> Result<Fir> // infaillible?

mod constraint_builder;
mod mono;

pub use constraint_builder::ConstraintBuilder;
pub use mono::Monomorphize;
