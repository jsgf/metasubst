use super::*;
use maplit::hashmap;
use proptest::proptest;

#[test]
fn simple() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {bar} you see", &mapping).unwrap(),
        "my FOO is rab you see"
    );
}

#[test]
fn end() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {bar}", &mapping).unwrap(),
        "my FOO is rab"
    );
}

#[test]
fn missing() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {missing} is {bar}", &mapping),
        Err(Error::MissingKeys {
            result: "my  is rab".to_string(),
            missing: vec!["missing".to_string()]
        })
    );
}
#[test]
fn emptystr() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(subst("", &mapping).unwrap(), "");
}

#[test]
fn incomplete() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {bar", &mapping),
        Err(Error::ShortInput {
            result: "my FOO is ".to_string(),
            state: r#"Var("bar")"#.to_string(),
        })
    );
}

#[test]
fn incomplete_open() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {", &mapping),
        Err(Error::ShortInput {
            result: "my FOO is ".to_string(),
            state: r#"(Text, Var(""))"#.to_string(),
        })
    );
}

#[test]
fn incomplete_params() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {bar:1", &mapping),
        Err(Error::ShortInput {
            result: "my FOO is ".to_string(),
            state: r#"Param("bar", ["1"])"#.to_string(),
        })
    );
}

#[test]
fn quotes() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {{foo}} is {foo}", &mapping).unwrap(),
        "my {foo} is FOO"
    );
}

#[test]
fn quotevar() {
    let mapping = hashmap! {
        "fo{o" => "FOO",
        "fo}o" => "OOF",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {fo{{o} is {bar}", &mapping).unwrap(),
        "my FOO is rab"
    );
    assert_eq!(
        subst("my {fo}}o} is {bar}", &mapping).unwrap(),
        "my OOF is rab"
    );
}

#[test]
fn empty() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {}.", &mapping).unwrap(),
        "my FOO is <empty>."
    );
}

#[test]
fn empty_end() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo} is {}", &mapping).unwrap(),
        "my FOO is <empty>"
    );
}

#[test]
fn params() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo:thing,that} is {bar}", &mapping).unwrap(),
        "my FOO is rab"
    );
}

#[test]
fn quote_params() {
    let mapping = hashmap! {
        "foo:thing,that" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo::thing,that} is {bar}", &mapping).unwrap(),
        "my FOO is rab"
    );
}

#[test]
fn params_empty() {
    let mapping = hashmap! {
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {foo:} is {bar}", &mapping).unwrap(),
        "my FOO is rab"
    );
}

#[test]
fn empty_params() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {:thing,bar} is {bar}", &mapping).unwrap(),
        "my <empty> is rab"
    );
}

#[test]
fn empty_params_empty() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    assert_eq!(
        subst("my {:} is {bar}", &mapping).unwrap(),
        "my <empty> is rab"
    );
}

#[test]
fn quotyquote() {
    assert_eq!(
        subst(
            "{{{{{{{{::}}",
            |_: &str, _: &[String]| -> Result<Option<&str>, Void> { Ok(None) }
        )
        .unwrap(),
        "{{{{::}"
    );
}

#[test]
fn some_params() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    let s = subst(
        "foo {foo:1,2,3} bar {bar:}",
        |key: &str, params: &[String]| -> Result<Option<String>, Void> {
            Ok(mapping
                .get(key)
                .map(|v| format!("v:{} p:{:?}", v, params).into()))
        },
    )
    .unwrap();
    assert_eq!(s, r#"foo v:FOO p:["1", "2", "3"] bar v:rab p:[]"#);
}

#[test]
fn param_quote_comma() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    let s = subst(
        "foo {foo:1,,2,3} bar {bar:}",
        |key: &str, params: &[String]| -> Result<Option<String>, Void> {
            Ok(mapping
                .get(key)
                .map(|v| format!("v:{} p:{:?}", v, params).into()))
        },
    )
    .unwrap();
    assert_eq!(s, r#"foo v:FOO p:["1,2", "3"] bar v:rab p:[]"#);
}

#[test]
fn param_quote_comma_empty() {
    let mapping = hashmap! {
        "" => "<empty>",
        "foo" => "FOO",
        "bar" => "rab",
    };

    let s = subst(
        "foo {foo:#?} bar {bar:}",
        |key: &str, params: &[String]| -> Result<Option<String>, Void> {
            Ok(mapping
                .get(key)
                .map(|v| format!("v:{} p:{:?}", v, params).into()))
        },
    )
    .unwrap();
    assert_eq!(s, r##"foo v:FOO p:["#?"] bar v:rab p:[]"##);
}

proptest! {
    #[test]
    // Check we don't get bad parser states in the format string
    fn doesnt_crash(fmtstr: String) {
        let mapping: HashMap<String, String> = HashMap::new();

        let _ = subst(fmtstr, &mapping);
    }
}
