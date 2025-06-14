use crate::expr::Expr;

/// Substitute variable at depth `depth` with value `val`
pub fn subst(expr: Expr, depth: usize, val: &Expr) -> Expr {
    match expr {
        Expr::Var(i) => {
            if i == depth {
                shift(val.clone(), 0, depth)
            } else {
                Expr::Var(i)
            }
        }
        Expr::Lam(body) => Expr::Lam(Box::new(subst(*body, depth + 1, val))),
        Expr::App(f, arg) => Expr::App(
            Box::new(subst(*f, depth, val)),
            Box::new(subst(*arg, depth, val)),
        ),
        Expr::TypeUniverse => Expr::TypeUniverse,
        Expr::Typeof(expr) => Expr::Typeof(Box::new(subst(*expr, depth, val))),
        Expr::Pi(domain, codomain) => Expr::Pi(
            Box::new(subst(*domain, depth, val)),
            Box::new(subst(*codomain, depth, val)),
        ),
    }
}

/// Shifts De Bruijn indices of all variables >= cutoff by amount
pub fn shift(expr: Expr, amount: isize, cutoff: usize) -> Expr {
    match expr {
        Expr::Var(i) => {
            if i >= cutoff {
                Expr::Var((i as isize + amount) as usize)
            } else {
                Expr::Var(i)
            }
        }
        Expr::Lam(body) => Expr::Lam(Box::new(shift(*body, amount, cutoff + 1))),
        Expr::App(f, arg) => Expr::App(
            Box::new(shift(*f, amount, cutoff)),
            Box::new(shift(*arg, amount, cutoff)),
        ),
        Expr::TypeUniverse => Expr::TypeUniverse,
        Expr::Typeof(expr) => Expr::Typeof(Box::new(shift(*expr, amount, cutoff))),
        Expr::Pi(domain, codomain) => Expr::Pi(
            Box::new(shift(*domain, amount, cutoff)),
            Box::new(shift(*codomain, amount, cutoff)),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_function() {
        // (\x. x) y ==> y
        let id = Expr::Lam(Box::new(Expr::Var(0)));
        let arg = Expr::Var(42); // dummy external var
        let app = Expr::App(Box::new(id), Box::new(arg.clone()));

        let result = app.eval();
        assert_eq!(result, arg);
    }

    #[test]
    fn constant_function() {
        // (\x. \y. x) a b ==> a
        let const_fn = Expr::Lam(Box::new(Expr::Lam(Box::new(Expr::Var(1)))));
        let a = Expr::Var(100);
        let b = Expr::Var(200);
        let app = Expr::App(
            Box::new(Expr::App(Box::new(const_fn), Box::new(a.clone()))),
            Box::new(b.clone()),
        );

        let result = app.eval();
        assert_eq!(result, a);
    }
}
