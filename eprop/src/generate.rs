use crate::data;




pub(crate) fn generate_sigs(string: &str) -> Vec<data::Function> {
    let file = syn::parse_file(string).unwrap();

    let functions = file.items.iter().filter_map(|item| {
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
            Some(data::Function::from(func))
        } else {
            None
        }
    }).collect();

    functions
}


pub(crate) fn generate_tests(function: &data::Function, test_count: usize) -> Vec<data::ArbitraryFunction> {
    let mut tests: Vec<data::ArbitraryFunction> = Vec::new();
    for _ in 0..test_count {
        let function = function.clone();
        tests.push(function.into());
    }
    tests
}

pub(crate) fn generate_test_string(tests: Vec<data::ArbitraryFunction>) -> String {
    let mut test_string = String::new();
    for test in tests {
        test_string.push_str(&test.to_string());
        test_string.push_str("\n");
    }
    test_string
}
