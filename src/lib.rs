use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::error;

#[cfg(test)]
mod test;

#[derive(Debug, Clone, thiserror::Error, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Error<E: error::Error> {
    #[error("malformed format string: {0}")]
    BadFormat(String),
    #[error("missing keys in substitution: {missing:?}")]
    MissingKeys {
        result: String,
        missing: Vec<String>,
    },
    #[error("input string too short: {state}")]
    ShortInput { result: String, state: String },
    #[error("substitution error: {0}")]
    Subst(E),
}

#[derive(Copy, Clone, Debug, thiserror::Error, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Void {}

pub trait Substitute<'a, R = &'a str, E = Void>
where
    Cow<'a, str>: From<R>,
{
    fn subst(&self, key: &str) -> Option<R>;
    fn subst_params(&self, key: &str, _params: &[String]) -> Result<Option<R>, E> {
        Ok(self.subst(key))
    }
}

impl<'a> Substitute<'a> for &'a HashMap<String, String> {
    fn subst(&self, key: &str) -> Option<&'a str> {
        self.get(key).map(String::as_str)
    }
}

impl<'a, 'v> Substitute<'v> for &'a HashMap<&'a str, &'v str> {
    fn subst(&self, key: &str) -> Option<&'v str> {
        self.get(key).copied()
    }
}

impl<'a> Substitute<'a> for &'a BTreeMap<String, String> {
    fn subst(&self, key: &str) -> Option<&'a str> {
        self.get(key).map(String::as_str)
    }
}

impl<'a, 'v> Substitute<'v> for &'a BTreeMap<&'a str, &'v str> {
    fn subst(&self, key: &str) -> Option<&'v str> {
        self.get(key).copied()
    }
}

impl<'a, F, R, E> Substitute<'a, R, E> for F
where
    F: Fn(&str, &[String]) -> Result<Option<R>, E>,
    Cow<'a, str>: From<R>,
    E: std::fmt::Debug,
{
    fn subst_params(&self, key: &str, params: &[String]) -> Result<Option<R>, E> {
        self(key, params)
    }

    fn subst(&self, key: &str) -> Option<R> {
        self(key, &[]).expect("Substitution failed")
    }
}

fn extend_params(params: &mut Vec<String>, c: char) {
    match params.last_mut() {
        None => {
            let mut s = String::new();
            s.push(c);
            params.push(s)
        }
        Some(last) => last.push(c),
    }
}

pub fn subst<'a, S, M, R, E>(input: S, subst: M) -> Result<String, Error<E>>
where
    S: AsRef<str>,
    M: Substitute<'a, R, E>,
    Cow<'a, str>: From<R>,
    E: error::Error,
{
    let input = input.as_ref();
    let mut res = String::with_capacity(input.len());
    let mut missing = Vec::new();

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum State {
        /// Main string body
        Text,
        /// A possible quote (repeated metacharacter) with next or prev state
        Quote {
            quote: char,
            prev: Box<State>,
            next: Box<State>,
        },
        /// Accumulating variable name
        Var(String),
        /// Accumulating (skipping) parameters
        Param(String, Vec<String>),
    }
    use State::*;

    let mut state = Text;

    for c in input.chars() {
        state = match (state, c) {
            (st @ Text, '{') => Quote {
                quote: c,
                prev: Box::new(st),
                next: Box::new(Var(String::new())),
            },
            (Text, '}') => Quote {
                // handle }} after {{
                quote: c,
                prev: Box::new(Text),
                next: Box::new(Text),
            },
            (st @ Text, c) => {
                res.push(c);
                st
            }
            (Quote { quote, next, prev }, c) => {
                if c != quote {
                    // Not doubled so not quoted. Consume the metacharacter and use the current character for something
                    match (c, *prev, *next) {
                        // General `}` on var or param
                        (c, Var(var), Text) => {
                            match subst.subst(&var) {
                                Some(s) => res.push_str(&Cow::from(s)),
                                None => missing.push(var.to_string()),
                            }
                            res.push(c);
                            Text
                        }
                        (c, Param(var, params), Text) => {
                            match subst
                                .subst_params(&var, &params)
                                .map_err(Error::<E>::Subst)?
                            {
                                Some(s) => res.push_str(&Cow::from(s)),
                                None => missing.push(var.to_string()),
                            }
                            res.push(c);
                            Text
                        }
                        // `}` after empty param
                        ('}', Var(var), Param(_, params)) => Quote {
                            quote: c,
                            prev: Box::new(Param(var, params)),
                            next: Box::new(Text),
                        },
                        // general param after `:`
                        (_, Var(var), Param(_, mut params)) => {
                            extend_params(&mut params, c);
                            Param(var, params)
                        }
                        // `}` after empty var
                        ('}', Text, st @ Var(_)) => Quote {
                            quote: c,
                            prev: Box::new(st),
                            next: Box::new(Text),
                        },
                        // `:` after empty var
                        (':', Text, Var(var)) => Quote {
                            quote: ':',
                            prev: Box::new(Var(var.clone())),
                            next: Box::new(Param(var, vec![])),
                        },
                        // `{` to start var
                        (c, Text, Var(mut var)) => {
                            var.push(c);
                            Var(var)
                        }
                        // `,` to add param
                        (',', Param(_var, _params), Param(var, mut params)) => {
                            params.push(String::new());
                            Param(var, params)
                        }
                        // add char to param
                        (c, Param(_var, _params), Param(var, mut params)) => {
                            extend_params(&mut params, c);
                            Param(var, params)
                        }
                        // Syntax errors
                        (_, Var(_), Var(_))  // "{ {x"
                        | (_, Text, Text)  // "}x"
                            => return Err(Error::BadFormat(input.to_string())),

                        // Unreachable transitions
                        (_, prev @ Quote { .. }, next)
                        | (_, prev, next @ Quote { .. })
                        | (_, prev @ Text, next @ Param { .. })
                        | (_, prev @ Param(_, _), next @ Var(_)) => panic!(
                            "Bad transition: c = {:?} prev {:?} next {:?}",
                            c, prev, next
                        ),
                    }
                } else {
                    let mut prev = *prev;
                    match &mut prev {
                        Text => res.push(c),
                        Var(ref mut var) => var.push(c),
                        Param(_var, ref mut params) => extend_params(params, c),
                        Quote { .. } => panic!("Quote in Quote"),
                    };
                    prev
                }
            }
            (Var(var), ':') => Quote {
                quote: ':',
                prev: Box::new(Var(var.clone())),
                next: Box::new(Param(var, vec![])),
            },
            (st @ Var(_), '}') | (st @ Param(_, _), '}') => Quote {
                quote: c,
                prev: Box::new(st),
                next: Box::new(Text),
            },
            (st @ Var(_), '{') | (st @ Param(_, _), '{') => Quote {
                quote: c,
                prev: Box::new(st.clone()),
                next: Box::new(st),
            },
            (Var(mut var), c) => {
                var.push(c);
                Var(var)
            }
            (Param(var, mut params), ',') => Quote {
                quote: ',',
                prev: Box::new(Param(var.clone(), params.clone())),
                next: {
                    params.push(String::new());
                    Box::new(Param(var, params))
                },
            },
            (st @ Param(_, _), ':') => Quote {
                quote: ':',
                prev: Box::new(st.clone()),
                next: Box::new(st),
            },
            (Param(var, mut params), c) => {
                extend_params(&mut params, c);
                Param(var, params)
            }
        }
    }

    if let Quote { prev, next, .. } = state {
        match (*prev, *next) {
            (Var(var), Text) => match subst.subst(&var) {
                Some(s) => res.push_str(&Cow::from(s)),
                None => missing.push(var.to_string()),
            },
            (Param(var, params), Text) => {
                match subst.subst_params(&var, &params).map_err(Error::Subst)? {
                    Some(s) => res.push_str(&Cow::from(s)),
                    None => missing.push(var.to_string()),
                }
            }
            short => {
                return Err(Error::ShortInput {
                    result: res,
                    state: format!("{:?}", short),
                })
            }
        }
    }

    if missing.is_empty() {
        Ok(res)
    } else {
        Err(Error::MissingKeys {
            result: res,
            missing,
        })
    }
}