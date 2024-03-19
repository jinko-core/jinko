use std::marker::PhantomData;

use colored::Colorize;
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};

pub struct FirDebug<T, F>
where
    F: Fn(&T) -> String,
{
    header: Option<&'static str>,
    show_data: bool,
    data_fmt: Option<F>,
    _phantom: Option<PhantomData<T>>,
}

impl<T, F: Fn(&T) -> String> Default for FirDebug<T, F> {
    fn default() -> FirDebug<T, F> {
        FirDebug {
            header: None,
            show_data: false,
            data_fmt: None,
            _phantom: None,
        }
    }
}

fn fmt_r(refidx: &RefIdx) -> String {
    match refidx {
        RefIdx::Unresolved => "Unresolved".yellow().to_string(),
        RefIdx::Resolved(OriginIdx(value)) => {
            format!("Resolved -> {}", value.to_string().green())
        }
    }
}

fn fmt_opt(refidx: &Option<RefIdx>) -> String {
    match refidx {
        None => String::from("None"),
        Some(r) => fmt_r(r),
    }
}

fn fmt_slice(refidxes: &[RefIdx]) -> String {
    if refidxes.is_empty() {
        return String::from("[]");
    }

    let mut s = String::from("[ ");

    if let Some(r) = refidxes.first() {
        s.push_str(&fmt_r(r))
    }
    refidxes.iter().skip(1).for_each(|r| {
        s.push_str(", ");
        s.push_str(&fmt_r(r));
    });

    s.push_str(" ]");

    s
}

impl<T, F: Fn(&T) -> String> FirDebug<T, F> {
    pub fn header(self, header: &'static str) -> FirDebug<T, F> {
        FirDebug {
            header: Some(header),
            ..self
        }
    }

    pub fn show_data(self, formatter: F) -> FirDebug<T, F> {
        FirDebug {
            show_data: true,
            data_fmt: Some(formatter),
            ..self
        }
    }

    fn display_data(&self, data: &T) {
        eprintln!(
            "\t{}: {}\n",
            "data".green(),
            self.data_fmt.as_ref().unwrap()(data)
        )
    }

    fn display_kind(&self, kind: &Kind) {
        let str = match kind {
            Kind::Constant(_) => "Constant".blue(),
            Kind::TypeReference(_) => "TypeReference".blue(),
            Kind::NodeRef { .. } => "NodeRef".blue(),
            Kind::Generic { .. } => "Generic".blue(),
            Kind::RecordType { .. } => "RecordType".blue(),
            Kind::UnionType { .. } => "UnionType".blue(),
            Kind::Function { .. } => "Function".blue(),
            Kind::Binding { .. } => "Binding".blue(),
            Kind::Assignment { .. } => "Assignment".blue(),
            Kind::Instantiation { .. } => "Instantiation".blue(),
            Kind::TypeOffset { .. } => "TypeOffset".blue(),
            Kind::Call { .. } => "Call".blue(),
            Kind::Conditional { .. } => "Conditional".blue(),
            Kind::Loop { .. } => "Loop".blue(),
            Kind::Statements(_) => "Statements".blue(),
            Kind::Return(_) => "Return".blue(),
        };

        let content = match kind {
            Kind::Constant(r) => format!("0: {}", fmt_r(r)),
            Kind::TypeReference(r) => format!("0: {}", fmt_r(r)),
            Kind::NodeRef(to) => format!("0: {}", fmt_r(to)),
            Kind::Generic { default } => format!("default: {}", fmt_opt(default)),
            Kind::RecordType { generics, fields } => format!(
                "generics: {}\n\t\tfields: {}",
                fmt_slice(generics),
                fmt_slice(fields)
            ),
            Kind::UnionType { generics, variants } => format!(
                "generics: {}\n\t\tvariants: {}",
                fmt_slice(generics),
                fmt_slice(variants)
            ),
            Kind::Function {
                generics,
                args,
                return_type,
                block,
            } => format!(
                "generics: {}\n\t\targs: {}\n\t\treturn_type: {}\n\t\tblock: {}",
                fmt_slice(generics),
                fmt_slice(args),
                fmt_opt(return_type),
                fmt_opt(block)
            ),
            Kind::Binding { to, ty } => format!("to: {}\n\t\tty: {}", fmt_opt(to), fmt_opt(ty)),
            Kind::Assignment { to, from } => {
                format!("to: {}\n\t\tfrom: {}", fmt_r(to), fmt_r(from))
            }
            Kind::Instantiation {
                to,
                generics,
                fields,
            } => format!(
                "to: {}\n\t\tgenerics: {}\n\t\tfields: {}",
                fmt_r(to),
                fmt_slice(generics),
                fmt_slice(fields)
            ),
            Kind::TypeOffset { instance, field } => {
                format!("instance: {}\n\t\tfield: {}", fmt_r(instance), fmt_r(field))
            }
            Kind::Call { to, generics, args } => format!(
                "to: {}\n\t\tgenerics: {}\n\t\targs: {}",
                fmt_r(to),
                fmt_slice(generics),
                fmt_slice(args)
            ),
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => format!(
                "condition: {}\n\t\ttrue_block: {}\n\t\tfalse_block: {}",
                fmt_r(condition),
                fmt_r(true_block),
                fmt_opt(false_block)
            ),
            Kind::Loop { condition, block } => format!(
                "condition: {}\n\t\tblock: {}",
                fmt_r(condition),
                fmt_r(block)
            ),
            Kind::Statements(s) => format!("0: {}", fmt_slice(s)),
            Kind::Return(e) => format!("0: {}", fmt_opt(e)),
        };

        eprintln!("{str} {{");
        eprintln!("\t\t{content}");
        eprintln!("\t}},");
    }

    fn display_node(&self, node: &Node<T>) {
        let origin = node.origin.0.to_string().green();

        eprint!("\t{origin}: ");

        self.display_kind(&node.kind);
        match self.show_data {
            true => self.display_data(&node.data),
            false => eprintln!(),
        }
    }

    pub fn display(&self, fir: &Fir<T>) {
        eprintln!(
            "{}{}: [",
            "Fir".purple(),
            self.header
                .map_or(String::new(), |header| format!(" ({header})"))
        );
        fir.nodes.iter().for_each(|(_, node)| {
            self.display_node(node);
        });
        eprintln!("]");
    }
}
