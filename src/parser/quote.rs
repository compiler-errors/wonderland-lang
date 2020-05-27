#[macro_export]
macro_rules! cheshire_quote {
    ($program:expr, $fmt:tt, $( $item:ident = $value:expr ),* $(,)?) => {{
        use crate::parser::display::CheshireFormattable;

        let program = $program;
        let new_program = std::mem::replace(program, ::wonderland::ana::represent::AnalyzedProgram::empty());

        let f = ::wonderland::util::FileRegistry::register_formatted_string(
            "quasiquote",
            format_args!($fmt, $( $item = ($value).cheshire_display() ),*)
        )?;

        let contents = ::wonderland::util::FileRegistry::open(f)?;
        let lex = ::wonderland::lexer::Lexer::new(f, &contents, true);

        let obj = ::wonderland::parser::parse(f, "quasiquote", lex)?;
        let (mut new_program, obj) = ::wonderland::ana::analyze_item(new_program, obj)?;

        // Swap the program back!
        std::mem::swap(program, &mut new_program);

        obj
    }}
}
