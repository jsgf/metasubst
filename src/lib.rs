//! Expand variables in a string
//!
//! This crate exposes a single function - `subst` - which takes a string format and
//! expands any variable references in it. It consults an implementation of the `Substitute`
//! trait to find the expansions. By default this is implemented for `HashMap` and `BTreeMap`,
//! and for closures.
use std::borrow::{Borrow, Cow};
use std::collections::{BTreeMap, HashMap};
use std::error;
use std::hash::{BuildHasher, Hash};

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

impl<'a: 'v, 'v, K, V, S> Substitute<'v> for &'a HashMap<K, V, S>
where
    K: Eq + Hash + Borrow<str>,
    V: AsRef<str> + 'v,
    S: BuildHasher,
{
    fn subst(&self, key: &str) -> Option<&'v str> {
        self.get(key).map(|v| v.as_ref())
    }
}

impl<'a: 'v, 'v, K, V> Substitute<'v> for &'a BTreeMap<K, V>
where
    K: Eq + Ord + Borrow<str>,
    V: AsRef<str> + 'v,
{
    fn subst(&self, key: &str) -> Option<&'v str> {
        self.get(key).map(|v| v.as_ref())
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
/// For example:
/// ```rust
/// # use metasubst::subst;
/// let map = maplit::hashmap! {
///     "foo" => "FOO",
///     "bar" => "blacksheep",
/// };
///
/// let out = subst("bar bar {bar}", &map).unwrap();
/// assert_eq!(out, "bar bar blacksheep");
/// ```
///
/// On success, it returns a new `String` with all the substitutions. If any
/// substitutions are missing, it returns an error with the final formatted string,
/// and all the missing variables.
///
/// Substitutions can also have parameters, such as `{variable:param1,param3}`. Parameters
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
        Base(BaseState),
        /// A possible quote (repeated metacharacter) with next or prev state
        Quote {
            quote: char,
            prev: BaseState,
            next: BaseState,
        },
    }
    use State::*;
    #[derive(Clone, Debug, Eq, PartialEq)]
    enum BaseState {
        /// Main string body
        Text,
        /// Accumulating variable name
        Var(String),
        /// Accumulating parameters
        Param(String, Vec<String>),
    }
    use BaseState::*;

    let mut state = Base(Text);

    for c in input.chars() {
        state = match (state, c) {
            (Base(st @ Text), '{') => Quote {
                quote: c,
                prev: st,
                next: Var(String::new()),
            },
            (Base(Text), '}') => Quote {
                // handle }} after {{
                quote: c,
                prev: Text,
                next: Text,
            },
            (st @ Base(Text), c) => {
                res.push(c);
                st
            }
            (Base(Var(var)), ':') => Quote {
                quote: ':',
                prev: Var(var.clone()),
                next: Param(var, vec![]),
            },
            (Base(st @ Var(_)), '}') | (Base(st @ Param(_, _)), '}') => Quote {
                quote: c,
                prev: st,
                next: Text,
            },
            (Base(st @ Var(_)), '{') | (Base(st @ Param(_, _)), '{') => Quote {
                quote: c,
                prev: st.clone(),
                next: st,
            },
            (Base(Var(mut var)), c) => {
                var.push(c);
                Base(Var(var))
            }
            (Base(Param(var, mut params)), ',') => Quote {
                quote: ',',
                prev: (Param(var.clone(), params.clone())),
                next: {
                    params.push(String::new());
                    (Param(var, params))
                },
            },
            (Base(st @ Param(_, _)), ':') => Quote {
                quote: ':',
                prev: st.clone(),
                next: st,
            },
            (Base(Param(var, mut params)), c) => {
                extend_params(&mut params, c);
                Base(Param(var, params))
            }
            (
                Quote {
                    quote, mut prev, ..
                },
                c,
            ) if c == quote => {
                // Repeated meta-character = quoted. Emit metacharacter and
                // return to previous state.
                match &mut prev {
                    Text => res.push(c),
                    Var(ref mut var) => var.push(c),
                    Param(_var, ref mut params) => extend_params(params, c),
                };
                Base(prev)
            }
            (Quote { prev, next, .. }, c) => {
                // Not doubled so not quoted.
                // Consume the metacharacter, move to the next state, and apply
                // current character to that state.
                match (c, prev, next) {
                    // General `}` on var or param
                    (c, Var(var), Text) => {
                        match subst.subst(&var) {
                            Some(s) => res.push_str(&Cow::from(s)),
                            None => missing.push(var.to_string()),
                        }
                        res.push(c);
                        Base(Text)
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
                        Base(Text)
                    }
                    // `}` after empty param
                    ('}', Var(var), Param(_, params)) => Quote {
                        quote: c,
                        prev: Param(var, params),
                        next: Text,
                    },
                    // general param after `:`
                    (_, Var(var), Param(_, mut params)) => {
                        extend_params(&mut params, c);
                        Base(Param(var, params))
                    }
                    // `}` after empty var
                    ('}', Text, st @ Var(_)) => Quote {
                        quote: c,
                        prev: st,
                        next: Text,
                    },
                    // `:` after empty var
                    (':', Text, Var(var)) => Quote {
                        quote: ':',
                        prev: Var(var.clone()),
                        next: Param(var, vec![]),
                    },
                    // `{` to start var
                    (c, Text, Var(mut var)) => {
                        var.push(c);
                        Base(Var(var))
                    }
                    // `,` to add param
                    (',', Param(_var, _params), Param(var, mut params)) => {
                        params.push(String::new());
                        Base(Param(var, params))
                    }
                    // add char to param
                    (c, Param(_var, _params), Param(var, mut params)) => {
                        extend_params(&mut params, c);
                        Base(Param(var, params))
                    }
                    // Syntax errors
                    (_, Var(_), Var(_))  // "{ {x"
                    | (_, Text, Text)  // "}x"
                        => return Err(Error::BadFormat(input.to_string())),

                    // Unreachable transitions
                    (_, prev @ Text, next @ Param { .. })
                    | (_, prev @ Param(_, _), next @ Var(_)) => panic!(
                        "Bad transition: c = {:?} prev {:?} next {:?}",
                        c, prev, next
                    ),
                }
            }
        }
    }

    // Finalize state
    match state {
        Quote { prev, next, .. } => match (prev, next) {
            // Pending var substitution
            (Var(var), Text) => match subst.subst(&var) {
                Some(s) => res.push_str(&Cow::from(s)),
                None => missing.push(var.to_string()),
            },
            // Pending substitution with params
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
        Base(st @ Var(_)) | Base(st @ Param(..)) => {
            return Err(Error::ShortInput {
                result: res,
                state: format!("{:?}", st),
            })
        }
        Base(Text) => {}
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
