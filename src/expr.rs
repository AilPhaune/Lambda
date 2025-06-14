use crate::{eval::subst, parse::NamedExpr};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    TypeUniverse,
    Typeof(Box<Expr>),         // typeof expr
    Pi(Box<Expr>, Box<Expr>),  // Function type: A -> B
    Var(usize),                // Variable, represented by De Bruijn index
    Lam(Box<Expr>),            // Lambda abstraction: \x. e
    App(Box<Expr>, Box<Expr>), // Application: e1 e2
}

impl Expr {
    /// Evaluate an expression to weak head normal form (WHNF)
    pub fn eval(self) -> Expr {
        match self {
            Expr::App(f, arg) => {
                let f = f.eval();
                let arg = arg.eval();
                if let Expr::Lam(body) = f {
                    subst(*body, 0, &arg).eval()
                } else {
                    Expr::App(Box::new(f), Box::new(arg))
                }
            }
            Expr::Lam(body) => Expr::Lam(Box::new(body.eval())),
            Expr::Var(_) => self,
            Expr::TypeUniverse => self,
            Expr::Pi(..) => self,
            Expr::Typeof(expr) => Expr::Typeof(Box::new(expr.eval())),
        }
    }
}

/// Converts NamedExpr with variable names into Expr with De Bruijn indices.
/// `env` is a stack of variable names currently in scope.
/// Returns error string if a free variable is encountered.
pub fn named_to_debruijn(expr: &NamedExpr, env: &mut Vec<String>) -> Result<Expr, String> {
    match expr {
        NamedExpr::Var(name) => {
            // Find the variable's distance to the closest binding
            if let Some(pos) = env.iter().rev().position(|v| v == name) {
                Ok(Expr::Var(pos))
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        }
        NamedExpr::Lam(param, _, body) => {
            env.push(param.clone());
            let body_db = named_to_debruijn(body, env)?;
            env.pop();
            Ok(Expr::Lam(Box::new(body_db)))
        }
        NamedExpr::App(f, arg) => {
            let f_db = named_to_debruijn(f, env)?;
            let arg_db = named_to_debruijn(arg, env)?;
            Ok(Expr::App(Box::new(f_db), Box::new(arg_db)))
        }
        NamedExpr::Pi(binding, domain, codomain) => {
            let domain_db = named_to_debruijn(domain, env)?;
            env.push(binding.clone().unwrap_or("_".to_string()));
            let codomain_db = named_to_debruijn(codomain, env)?;
            env.pop();
            Ok(Expr::Pi(Box::new(domain_db), Box::new(codomain_db)))
        }
        NamedExpr::Type => Ok(Expr::TypeUniverse),
        NamedExpr::Typeof(expr) => {
            let expr_db = named_to_debruijn(expr, env)?;
            Ok(Expr::Typeof(Box::new(expr_db)))
        }
    }
}

pub fn expr_to_string(expr: &Expr, env: &mut Vec<String>) -> String {
    match expr {
        Expr::Var(idx) => env
            .get(env.len() - idx - 1)
            .cloned()
            .unwrap_or(format!("#{}", idx)),
        Expr::Lam(body) => {
            let var_name = format!("x{}", env.len());
            env.push(var_name.clone());
            let body_str = expr_to_string(body, env);
            env.pop();
            format!("(\\{}. {})", var_name, body_str)
        }
        Expr::App(f, arg) => {
            let f_str = expr_to_string(f, env);
            let arg_str = expr_to_string(arg, env);
            format!("({} {})", f_str, arg_str)
        }
        Expr::TypeUniverse => String::from("Type"),
        Expr::Pi(codomain, domain) => {
            let codomain_str = expr_to_string(codomain, env);
            let var_name = format!("x{}", env.len());
            env.push(var_name.clone());
            let domain_str = expr_to_string(domain, env);
            env.pop();
            format!("(({}: {}) -> {})", var_name, domain_str, codomain_str)
        }
        Expr::Typeof(expr) => {
            let expr_str = expr_to_string(expr, env);
            format!("typeof {}", expr_str)
        }
    }
}
