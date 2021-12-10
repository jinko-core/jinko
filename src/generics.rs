use crate::instruction::TypeId;

/// Format a function name to correspond to the low level implementation after generics
/// have been expanded
fn format_function_name(fn_name: &str, generics: Vec<TypeId>) -> String {
    generics
        .iter()
        .fold(format!("__{}", fn_name), |acc, type_id| {
            format!("{}_{}", acc, type_id.id())
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_generic() {
        let types = vec![TypeId::new(String::from("int"))];
        assert_eq!(format_function_name("f", types), "__f_int");
    }

    #[test]
    fn multi_generic() {
        let types = vec![
            TypeId::new(String::from("int")),
            TypeId::new(String::from("string")),
            TypeId::new(String::from("CustomType")),
        ];
        assert_eq!(
            format_function_name("f", types),
            "__f_int_string_CustomType"
        );
    }

    #[test]
    fn no_generic() {
        assert_eq!(format_function_name("f", vec![]), "__f");
    }
}
