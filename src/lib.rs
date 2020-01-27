//! Expand variables in a string
//!
//! This crate exposes a single function - `subst` - which takes a string format and
//! expands any variable references in it. It consults an implementation of the `Substitute`
//! trait to find the expansions. By default this is implemented for `HashMap` and `BTreeMap`,
//! and for closures.
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::error;

#[cfg(test)]
mod test;

/// Error type.
#[derive(Debug, Clone, thiserror::Error, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Error<E: error::Error + 'static> {
    /// Format string has a syntax error
    #[error("malformed format string: {0}")]
    BadFormat(String),
    /// Some substitutions were missing. The partial result is returned (missing substitutions
    /// are replaced with empty strings).
    #[error("missing keys in substitution: {missing:?}")]
    MissingKeys {
        result: String,
        missing: Vec<String>,
    },
    /// Input string was truncated in the middle of something (either in the middle of a
    /// variable or a quote).
    #[error("input string too short: {state}")]
    ShortInput { result: String, state: String },
    /// Substitution returned an error.
    #[error("substitution error: {0}")]
    Subst(#[source] E),
}

/// Error type for infallable substitutions
#[derive(Copy, Clone, Debug, thiserror::Error, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Void {}

/// Define substitutions for references in a format string.
pub trait Substitute<'a, R = &'a str, E = Void>
where
    Cow<'a, str>: From<R>,
{
    /// Simple substitution without parameters. The assumption is that this can't fail.
    fn subst(&self, key: &str) -> Option<R>;

    /// Substitution with parameters. This can fail if the parameters are wrong for
    /// a given implementation.
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

/// Given an input string, substitute values for any reference in the input.
/// The simplest form is just a variable name: `{variable}`. A substitution is
/// found by calling `subst.subst("variable")`. If no substitution is found, then
/// the variable is replaced with an empty string, and the missing name noted.
///
/// On success, it returns a new `String` with all the substitutions. If any
/// substitutions are missing, it returns an error with the final formatted string,
/// and all the missing variables.
///
/// Substitutions can also have parameters - `{variable:param1,param3}`. Parameters
/// are separated with `,`, but otherwise have no defined syntax. They are passed to
/// `Substitute::subst_param` which can apply any interpretation to the parameters
/// as it wants. It may also return errors if there's something wrong with the parameters.
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

    match state {
        Quote { prev, next, .. } => match (*prev, *next) {
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
        },
        st @ Var(_) | st @ Param(..) => {
            return Err(Error::ShortInput {
                result: res,
                state: format!("{:?}", st),
            })
        }
        Text => (),
    };

    if missing.is_empty() {
        Ok(res)
    } else {
        Err(Error::MissingKeys {
            result: res,
            missing,
        })
    }
}
