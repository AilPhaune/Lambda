use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
};

#[derive(Debug, Clone, PartialEq)]
pub enum NamedExpr {
    Var(String),
    Lam(String, Box<NamedExpr>, Box<NamedExpr>), // \x : type . expr
    App(Box<NamedExpr>, Box<NamedExpr>),
    Type,
    Typeof(Box<NamedExpr>),                             // typeof expr
    Pi(Option<String>, Box<NamedExpr>, Box<NamedExpr>), // type of Lam, (x: A) -> B; B may depend on x
}

// Parse identifiers (variables)
pub fn parse_ident(input: &str) -> IResult<&str, String> {
    let first_char = |c: char| c.is_alphabetic() || c == '_';
    let other_char = |c: char| c.is_alphanumeric() || c == '_';

    map(
        pair(take_while1(first_char), take_while(other_char)),
        |(first, rest): (&str, &str)| {
            let mut s = String::from(first);
            s.push_str(rest);
            s
        },
    )
    .parse(input)
}

// Parse "Type" keyword
pub fn parse_type_keyword(input: &str) -> IResult<&str, NamedExpr> {
    map(tag("Type"), |_| NamedExpr::Type).parse(input)
}

// Parse parenthesized expression
pub fn parse_parens(input: &str) -> IResult<&str, NamedExpr> {
    delimited(
        preceded(multispace0, char('(')),
        preceded(multispace0, parse_expr),
        preceded(multispace0, char(')')),
    )
    .parse(input)
}

// Parse atomic expressions: variable, Type, or parens
pub fn parse_atom(input: &str) -> IResult<&str, NamedExpr> {
    preceded(
        multispace0,
        alt((
            parse_type_keyword,
            parse_typeof,
            map(parse_ident, NamedExpr::Var),
            parse_parens,
        )),
    )
    .parse(input)
}

// Parse lambda abstraction: \x : type . expr
pub fn parse_lambda(input: &str) -> IResult<&str, NamedExpr> {
    let (input, _) = preceded(
        multispace0,
        alt((tag("\\"), terminated(tag("lambda"), multispace1))),
    )
    .parse(input)?;
    let (input, _) = multispace0(input)?;
    let (input, var) = parse_ident(input)?;
    let (input, _) = preceded(multispace0, char(':')).parse(input)?;
    let (input, typ) = parse_pi_type(input)?;
    let (input, _) = preceded(multispace0, char('.')).parse(input)?;
    let (input, body) = parse_expr(input)?;
    Ok((input, NamedExpr::Lam(var, Box::new(typ), Box::new(body))))
}

// Parse application: sequence of atoms or lambdas
pub fn parse_application(input: &str) -> IResult<&str, NamedExpr> {
    let (input, initial) = parse_atom(input)?;
    let (input, args) = many0(parse_atom).parse(input)?;

    let result = args.into_iter().fold(initial, |acc, arg| {
        NamedExpr::App(Box::new(acc), Box::new(arg))
    });

    Ok((input, result))
}

// Parse named binding: (x: type)
pub fn parse_named_binding(input: &str) -> IResult<&str, (String, NamedExpr)> {
    delimited(
        preceded(multispace0, char('(')),
        preceded(
            multispace0,
            pair(
                parse_ident,
                preceded(
                    preceded(multispace0, char(':')),
                    preceded(multispace0, parse_pi_type),
                ),
            ),
        ),
        preceded(multispace0, char(')')),
    )
    .parse(input)
}

// Pi type: (x: A) -> B
pub fn parse_pi_type(input: &str) -> IResult<&str, NamedExpr> {
    let (input, (binding, domain)) = alt((
        parse_named_binding.map(|(name, expr)| (Some(name), expr)),
        parse_application.map(|expr| (None, expr)),
    ))
    .parse(input)?;

    let (input, opt_arrow) = opt(preceded(
        preceded(multispace0, tag("->")),
        preceded(multispace0, parse_pi_type),
    ))
    .parse(input)?;

    if let Some(codomain) = opt_arrow {
        Ok((
            input,
            NamedExpr::Pi(binding, Box::new(domain), Box::new(codomain)),
        ))
    } else {
        Ok((input, domain))
    }
}

// typeof expr
pub fn parse_typeof(input: &str) -> IResult<&str, NamedExpr> {
    let (input, _) = preceded(multispace0, tag("typeof")).parse(input)?;
    let (input, expr) = preceded(multispace1, parse_atom).parse(input)?;
    Ok((input, NamedExpr::Typeof(Box::new(expr))))
}

// Parse any expression: lambda or application or atom
pub fn parse_expr(input: &str) -> IResult<&str, NamedExpr> {
    alt((parse_lambda, parse_pi_type)).parse(input)
}
