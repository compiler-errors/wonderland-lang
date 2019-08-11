use self::represent::*;
use crate::parser::*;
use crate::parser::{
    AstBlock, AstExpression, AstFunction, AstNamedVariable, AstStatement, ParseFile,
};
use crate::util::UniqueMap;
use crate::util::{Counter, StackMap};
use std::collections::HashMap;

mod analyze_generics;
mod analyze_infer;
mod analyze_self;
mod analyze_variables;
mod represent;
