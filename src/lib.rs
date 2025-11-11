use lalrpop_util::lalrpop_mod;
use aleph_syntax_tree::syntax::AlephTree as at;

lalrpop_mod!(pub grammar); 

/// Parse COBOL source to AlephTree.
///
/// # Arguments
/// * `source` - COBOL source code.
///
/// # Return
/// An `AlephTree`.
pub fn parse(source: String) -> at {
    let ast = grammar::ProgramParser::new().parse(&source);
    match ast {
        Ok(res) => res,
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            at::Unit
        }
    }
}

/// Parse a COBOL statements (for REPL and interactive).
///
/// # Arguments
/// * `source` - Cobol statements.
///
/// # Return
/// `Vec<Box<at>>` corresponding to Stmts.
pub fn parse_statements(source: String) -> Vec<Box<at>> {
    let ast = grammar::StatementListParser::new().parse(&source);
    match ast {
        Ok(res) => res,
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Vec::new()
        }
    }
}


