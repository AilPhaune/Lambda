use crate::{expr::named_to_debruijn, parse::NamedExpr};

type Context = Vec<(String, NamedExpr)>;

#[derive(Debug)]
pub enum TypeError {
    UnboundVar(String),
    NotAFunction(NamedExpr),
    NotAType(NamedExpr),
    Mismatch(NamedExpr, NamedExpr), // expected, actual
}

pub fn alpha_eq(e1: &NamedExpr, e2: &NamedExpr) -> bool {
    let mut ctx1 = vec![];
    let mut ctx2 = vec![];
    let db1 = named_to_debruijn(e1, &mut ctx1);
    let db2 = named_to_debruijn(e2, &mut ctx2);
    if let (Ok(db1), Ok(db2)) = (db1, db2) {
        db1 == db2
    } else {
        false
    }
}

pub fn subst_named(expr: &NamedExpr, replacement: &NamedExpr, var_name: &str) -> NamedExpr {
    match expr {
        NamedExpr::Var(n) if n == var_name => replacement.clone(),
        NamedExpr::Var(_) => expr.clone(),
        NamedExpr::Type => NamedExpr::Type,
        NamedExpr::Typeof(expr) => {
            NamedExpr::Typeof(Box::new(subst_named(expr, replacement, var_name)))
        }
        NamedExpr::Lam(var, typ, body) => {
            if var == var_name {
                expr.clone() // shadowed
            } else {
                NamedExpr::Lam(
                    var.clone(),
                    Box::new(subst_named(typ, replacement, var_name)),
                    Box::new(subst_named(body, replacement, var_name)),
                )
            }
        }
        NamedExpr::App(f, a) => NamedExpr::App(
            Box::new(subst_named(f, replacement, var_name)),
            Box::new(subst_named(a, replacement, var_name)),
        ),
        NamedExpr::Pi(binding, dom, codom) => {
            match binding {
                Some(name) if name == var_name => expr.clone(), // shadowed
                _ => NamedExpr::Pi(
                    binding.clone(),
                    Box::new(subst_named(dom, replacement, var_name)),
                    Box::new(subst_named(codom, replacement, var_name)),
                ),
            }
        }
    }
}

pub fn simplify_type(ctx: &mut Context, expr: &mut NamedExpr) -> Result<NamedExpr, TypeError> {
    let typ = type_check(ctx, expr)?;
    match typ {
        NamedExpr::Typeof(typ) => Ok(*typ),
        NamedExpr::Type => Ok(expr.clone()),
        _ => Err(TypeError::NotAType(expr.clone())),
    }
}

pub fn type_check(ctx: &mut Context, expr: &mut NamedExpr) -> Result<NamedExpr, TypeError> {
    match expr {
        NamedExpr::Var(name) => ctx
            .iter()
            .rev()
            .find(|v| &v.0 == name)
            .map(|v| v.1.clone())
            .ok_or(TypeError::UnboundVar(name.clone())),

        NamedExpr::Type => Ok(NamedExpr::Typeof(Box::new(NamedExpr::Type))),

        NamedExpr::Typeof(inner_expr) => {
            let expr_type = type_check(ctx, inner_expr)?;
            *expr = expr_type.clone();
            Ok(NamedExpr::Typeof(Box::new(expr_type)))
        }

        NamedExpr::Lam(var, typ, body) => {
            // Check type of parameter is a valid type
            let typ = simplify_type(ctx, typ)?;

            // Add (var : typ) to the context
            ctx.push((var.clone(), typ.clone()));
            // Check body type in extended context
            let body_type = type_check(ctx, body)?;
            // Pop the context after checking body
            ctx.pop();

            Ok(NamedExpr::Pi(
                Some(var.clone()),
                Box::new(typ),
                Box::new(body_type),
            ))
        }

        NamedExpr::App(fun, arg) => {
            // Infer function type
            let fun_type = type_check(ctx, fun)?;

            match &fun_type {
                NamedExpr::Pi(binding, dom, codom) => {
                    // Check argument type matches domain
                    let arg_type = type_check(ctx, arg)?;
                    if alpha_eq(&arg_type, dom) {
                        // Substitute argument for bound variable in codomain type
                        let bound_var = binding.clone().unwrap_or("_".to_string());
                        let codom_subst = subst_named(codom, arg, &bound_var);
                        Ok(codom_subst)
                    } else {
                        Err(TypeError::Mismatch((**dom).clone(), arg_type))
                    }
                }
                _ => Err(TypeError::NotAFunction(fun_type)),
            }
        }

        NamedExpr::Pi(binding, dom, codom) => {
            // Check domain is a type
            let dom = simplify_type(ctx, dom)?;
            // Extend context with binding to domain type
            ctx.push((binding.clone().unwrap_or("_".to_string()), dom.clone()));
            // Check codomain is a type in extended context
            let codom = simplify_type(ctx, codom)?;
            // Pop context
            ctx.pop();

            Ok(NamedExpr::Typeof(Box::new(NamedExpr::Pi(
                binding.clone(),
                Box::new(dom),
                Box::new(codom),
            ))))
        }
    }
}
