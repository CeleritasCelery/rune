use crate::code::data;

#[allow(dead_code)]
pub(crate) fn generate_sigs(string: &str, function_name: Option<&str>) -> Vec<data::Function> {
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
                data::Function::from_item(func).ok()
            } else {
                None
            }
        })
        .collect();

    functions
}
