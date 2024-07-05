use crate::data;

pub(crate) fn generate_sigs(string: &str, function_name: &Option<String>) -> Vec<data::Function> {
    let file = syn::parse_file(string).unwrap();

    let functions = file
        .items
        .iter()
        .filter_map(|item| {
            if let syn::Item::Fn(func) = item {
                let mut is_defun = false;
                for attr in &func.attrs {
                    if let syn::Meta::Path(path) = &attr.meta {
                        if let Some(ident) = path.get_ident() {
                            if ident == "defun" {
                                is_defun = true;
                            }
                        }
                    }
                }
                if !is_defun {
                    return None;
                }
                if let Some(name) = function_name {
                    if func.sig.ident != *name {
                        return None;
                    }
                }
                data::Function::from_item(func)
            } else {
                None
            }
        })
        .collect();

    functions
}

pub(crate) fn generate_tests(
    function: &data::Function,
    test_count: usize,
) -> Vec<data::ArbitraryFunction> {
    let mut tests: Vec<data::ArbitraryFunction> = Vec::new();
    for _ in 0..test_count {
        let function = function.clone();
        tests.push(function.into());
    }
    tests
}

pub(crate) fn generate_test_string<I: std::io::Write>(
    output: &mut I,
    tests: Vec<data::ArbitraryFunction>,
) {
    for test in tests {
        output.write_all(test.to_string().as_bytes()).unwrap();
        output.write_all(b"\n").unwrap();
    }
}
