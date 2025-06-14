use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    combinator::{map, opt},
    sequence::preceded,
};
use rustyline::{DefaultEditor, error::ReadlineError};

use crate::{
    data::MaybeRef,
    expr::{expr_to_string, named_to_debruijn},
    parse::{NamedExpr, parse_expr, parse_ident},
    type_check::{TypeError, alpha_eq, type_check},
};

#[derive(Debug)]
pub enum ReplAction {
    Eval(NamedExpr),
    LetBinding(String, Option<NamedExpr>, NamedExpr), // let x: type = expr
}

pub fn parse_repl_input_let_binding(input: &str) -> IResult<&str, ReplAction> {
    let (input, ident) = preceded(
        preceded(multispace0, tag("let")),
        preceded(multispace1, parse_ident),
    )
    .parse(input)?;

    let (input, typ) = opt(preceded(
        preceded(multispace0, tag(":")),
        preceded(multispace0, parse_expr),
    ))
    .parse(input)?;

    let (input, expr) = preceded(
        preceded(multispace0, tag(":=")),
        preceded(multispace0, parse_expr),
    )
    .parse(input)?;

    Ok((input, ReplAction::LetBinding(ident, typ, expr)))
}

pub fn parse_repl_input(input: &str) -> IResult<&str, ReplAction> {
    alt((
        parse_repl_input_let_binding,
        map(parse_expr, ReplAction::Eval),
    ))
    .parse(input)
}

pub fn repl() {
    let mut rl = DefaultEditor::new().unwrap();
    println!("Typed Lambda Calculus REPL. Type 'exit' or Ctrl+D to quit.");

    let mut let_bindings: Vec<(String, NamedExpr, NamedExpr)> = vec![];

    fn wrap_let_bindings<'a>(
        expr: &'a mut NamedExpr,
        bindings: &[(String, NamedExpr, NamedExpr)],
    ) -> MaybeRef<'a, NamedExpr> {
        if bindings.is_empty() {
            return MaybeRef::MutRef(expr);
        }

        let mut new_expr = expr.clone();

        for (name, typ, expr) in bindings.iter().rev() {
            new_expr = NamedExpr::Lam(name.clone(), Box::new(typ.clone()), Box::new(new_expr));
            new_expr = NamedExpr::App(Box::new(new_expr), Box::new(expr.clone()));
        }

        MaybeRef::Owned(new_expr)
    }

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line == "exit" {
                    break;
                }
                if line.is_empty() {
                    continue;
                }
                rl.add_history_entry(line).unwrap();

                match parse_repl_input(line) {
                    Ok((rest, action)) if rest.trim().is_empty() => match action {
                        ReplAction::Eval(mut named_expr) => {
                            let mut ctx = Vec::new();

                            let mut named_expr = wrap_let_bindings(&mut named_expr, &let_bindings);

                            match type_check(&mut ctx, named_expr.get_mut().unwrap()) {
                                Ok(res_type) => {
                                    let mut env = Vec::new();
                                    println!(": {:?}", res_type);
                                    match named_to_debruijn(named_expr.get(), &mut env) {
                                        Ok(expr) => {
                                            let evaluated = expr.clone().eval();
                                            let mut print_env = Vec::new();
                                            let result_str =
                                                expr_to_string(&evaluated, &mut print_env);
                                            println!("=> {}", result_str);
                                        }
                                        Err(e) => println!("Name resolution error: {}", e),
                                    }
                                }
                                Err(e) => println!("Type error: {:?}", e),
                            }
                        }
                        ReplAction::LetBinding(name, typ, mut expr) => {
                            let evaluated_type = {
                                let mut ctx = Vec::new();

                                match type_check(
                                    &mut ctx,
                                    wrap_let_bindings(&mut expr, &let_bindings)
                                        .get_mut()
                                        .unwrap(),
                                ) {
                                    Ok(res_type) => res_type,
                                    Err(e) => {
                                        println!("Type error: {:?}", e);
                                        continue;
                                    }
                                }
                            };

                            let Some(typ) = typ.map(|mut typ| {
                                let mut ctx = Vec::new();
                                match type_check(
                                    &mut ctx,
                                    wrap_let_bindings(&mut typ, &let_bindings)
                                        .get_mut()
                                        .unwrap(),
                                ) {
                                    Ok(NamedExpr::Typeof(res_type)) => Some(*res_type),
                                    Ok(v) => {
                                        println!("Type error: {:?}", TypeError::NotAType(v));
                                        None
                                    }
                                    Err(e) => {
                                        println!("Type error: {:?}", e);
                                        None
                                    }
                                }
                            }) else {
                                let_bindings.push((name, evaluated_type, expr));
                                continue;
                            };

                            if let Some(typ) = typ {
                                if !alpha_eq(&typ, &evaluated_type) {
                                    println!(
                                        "Type error: {:?}",
                                        TypeError::Mismatch(typ, evaluated_type)
                                    );
                                    continue;
                                }
                                let_bindings.push((name, typ, expr));
                            }
                        }
                    },
                    Ok((rest, _)) => println!("Parsing error: leftover input '{}'", rest),
                    Err(e) => println!("Parsing error: {:?}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
