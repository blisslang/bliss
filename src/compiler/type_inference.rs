use super::{Expr, Node, Type, TypeForm};
use lit_vek::iter;
use std::{
    collections::{HashMap, HashSet},
    error::Error,
};

struct Scheme {
    vars: Vec<String>,
    t: TypeForm,
}

type Env = HashMap<String, Scheme>;

#[derive(Default)]
pub struct TypeInference {
    uf: HashMap<String, TypeForm>,
    fresh_counter: i32,
}

impl TypeInference {
    fn find(&self, t: &TypeForm) -> TypeForm {
        match t {
            TypeForm::Variable { name } => match self.uf.get(name) {
                Some(uf_t) => self.find(uf_t),
                None => t.to_owned(),
            },
            _ => t.to_owned(),
        }
    }

    fn occurs(&self, var: &str, t: &TypeForm) -> bool {
        let t_rep = self.find(t);

        match t_rep {
            TypeForm::Variable { name } => var == name,
            TypeForm::Arrow { arg, ret } => self.occurs(var, &arg) || self.occurs(var, &ret),
            TypeForm::Record { fields } => fields
                .iter()
                .fold(false, |acc, (_, field_t)| acc || self.occurs(var, field_t)),
            TypeForm::List { items } => self.occurs(var, &items),
            _ => false,
        }
    }

    fn unify(&mut self, t1: &TypeForm, t2: &TypeForm) -> Result<(), Box<dyn Error>> {
        let t1_rep = self.find(t1);
        let t2_rep = self.find(t2);

        // If they are already the same we dont have to do anything
        if t1_rep != t2_rep {
            match (&t1_rep, &t2_rep) {
                (TypeForm::Variable { name: t1_name }, _) if !self.occurs(t1_name, &t2_rep) => {
                    self.uf.insert(t1_name.to_string(), t2_rep);
                }
                (_, TypeForm::Variable { name: t2_name }) if !self.occurs(t2_name, &t1_rep) => {
                    self.uf.insert(t2_name.to_string(), t1_rep);
                }

                (
                    TypeForm::Arrow {
                        arg: t1_arg,
                        ret: t1_ret,
                    },
                    TypeForm::Arrow {
                        arg: t2_arg,
                        ret: t2_ret,
                    },
                ) => {
                    self.unify(t1_arg, t2_arg)?;
                    self.unify(t1_ret, t2_ret)?;
                }

                (TypeForm::Constant { t: t1_t }, TypeForm::Constant { t: t2_t })
                    if t1_t == t2_t => {}

                (
                    TypeForm::Record { fields: t1_fields },
                    TypeForm::Record { fields: t2_fields },
                ) => {
                    let t1_keys = HashSet::<&String>::from_iter(t1_fields.keys());
                    let t2_keys = HashSet::<&String>::from_iter(t2_fields.keys());

                    let common_keys = t1_keys.intersection(&t2_keys);

                    for &key in common_keys {
                        let t1_field = t1_fields.get(key).ok_or("Field not found")?;
                        let t2_field = t2_fields.get(key).ok_or("Field not found")?;

                        self.unify(t1_field, t2_field)?;
                    }
                }

                (TypeForm::List { items: t1_items }, TypeForm::List { items: t2_items }) => {
                    self.unify(t1_items, t2_items)?;
                }

                (_, _) => {
                    return Err(format!(
                        "Mismatched types: Cannot use type {:#?} where type {:#?} was expected",
                        t2_rep, t1_rep
                    )
                    .into());
                }
            }
        }

        Ok(())
    }

    fn fresh_var(&mut self) -> TypeForm {
        let name = format!("a{}", self.fresh_counter);

        self.fresh_counter += 1;

        TypeForm::Variable { name }
    }

    fn apply_subst(&self, t: &TypeForm, subst: &HashMap<String, TypeForm>) -> TypeForm {
        match t {
            TypeForm::Variable { name } => subst.get(name).unwrap_or(t).to_owned(),

            TypeForm::Constant { t: _ } => t.to_owned(),

            TypeForm::Arrow { arg, ret } => TypeForm::Arrow {
                arg: self.apply_subst(arg, subst).into(),
                ret: self.apply_subst(ret, subst).into(),
            },

            TypeForm::Record { fields } => {
                let new_fields = fields
                    .iter()
                    .map(|(label, field_t)| (label.to_owned(), self.apply_subst(field_t, subst)))
                    .collect();

                TypeForm::Record { fields: new_fields }
            }

            TypeForm::List { items } => TypeForm::List {
                items: self.apply_subst(items, subst).into(),
            },
        }
    }

    fn inst(&mut self, scheme: &Scheme) -> TypeForm {
        let mut subst = HashMap::<String, TypeForm>::new();

        for var in &scheme.vars {
            let fresh_tvar = self.fresh_var();

            subst.insert(var.to_string(), fresh_tvar);
        }

        self.apply_subst(&scheme.t, &subst)
    }

    fn free_vars_type(&self, t: &TypeForm) -> HashSet<String> {
        let t_rep = self.find(t);

        match t_rep {
            TypeForm::Variable { name } => HashSet::from_iter(iter![name]),
            TypeForm::Constant { t: _ } => HashSet::new(),
            TypeForm::Arrow { arg, ret } => self
                .free_vars_type(&arg)
                .union(&self.free_vars_type(&ret))
                .cloned()
                .collect(),
            TypeForm::Record { fields } => fields.values().fold(HashSet::new(), |acc, field_t| {
                acc.union(&self.free_vars_type(field_t)).cloned().collect()
            }),
            TypeForm::List { items } => self.free_vars_type(&items),
        }
    }

    fn free_vars_scheme(&self, scheme: &Scheme) -> HashSet<String> {
        self.free_vars_type(&scheme.t)
            .difference(&HashSet::from_iter(scheme.vars.iter().cloned()))
            .cloned()
            .collect()
    }

    fn free_vars_context(&self, env: &Env) -> HashSet<String> {
        env.values().fold(HashSet::new(), |acc, scheme| {
            acc.union(&self.free_vars_scheme(scheme)).cloned().collect()
        })
    }

    fn generalize(&self, env: &Env, t: &TypeForm) -> Scheme {
        let fv_t = self.free_vars_type(t);
        let fv_env = self.free_vars_context(env);

        let quantified_vars = fv_t.difference(&fv_env).cloned().collect::<Vec<String>>();

        Scheme {
            vars: quantified_vars,
            t: t.to_owned(),
        }
    }

    fn infer_j(&mut self, env: &mut Env, e: &Expr) -> Result<(Expr, TypeForm), Box<dyn Error>> {
        match e {
            Expr::Unit { t: _ } => {
                let t = TypeForm::Constant { t: Type::Unit };

                Ok((
                    Expr::Unit {
                        t: Some(t.to_owned()),
                    },
                    t,
                ))
            }

            Expr::Number { t: _, value } => {
                let t = TypeForm::Constant { t: Type::Number };

                Ok((
                    Expr::Number {
                        t: Some(t.to_owned()),
                        value: value.to_owned(),
                    },
                    t,
                ))
            }
            Expr::String { t: _, value } => {
                let t = TypeForm::Constant { t: Type::String };

                Ok((
                    Expr::String {
                        t: Some(t.to_owned()),
                        value: value.to_owned(),
                    },
                    t,
                ))
            }
            Expr::Boolean { t: _, value } => {
                let t = TypeForm::Constant { t: Type::Boolean };

                Ok((
                    Expr::Boolean {
                        t: Some(t.to_owned()),
                        value: value.to_owned(),
                    },
                    t,
                ))
            }

            Expr::DoBlock { t: _, body } => {
                let (body_e, body_t) = self.infer_j(env, body)?;

                Ok((
                    Expr::DoBlock {
                        t: Some(body_t.to_owned()),
                        body: body_e.into(),
                    },
                    body_t,
                ))
            }

            Expr::Variable { t: _, var_name } => match env.get(var_name) {
                Some(scheme) => {
                    let t = self.inst(scheme);

                    Ok((
                        Expr::Variable {
                            t: Some(t.to_owned()),
                            var_name: var_name.to_owned(),
                        },
                        t,
                    ))
                }
                None => Err(format!(
                    "Unbound variable: No variable with name {} was found.",
                    var_name
                )
                .into()),
            },
            Expr::FunctionCall { t: _, fn_name, arg } => {
                let (fn_e, fn_t) = self.infer_j(env, fn_name)?;
                let (arg_e, arg_t) = self.infer_j(env, arg)?;
                let ret_t = self.fresh_var();

                let arrow = TypeForm::Arrow {
                    arg: arg_t.into(),
                    ret: ret_t.to_owned().into(),
                };

                self.unify(&fn_t, &arrow)?;

                Ok((
                    Expr::FunctionCall {
                        t: Some(ret_t.to_owned()),
                        fn_name: fn_e.into(),
                        arg: arg_e.into(),
                    },
                    ret_t,
                ))
            }

            Expr::LambdaFunction {
                t: _,
                arg_name,
                body,
            } => {
                let param_t = if arg_name.is_some() {
                    self.fresh_var()
                } else {
                    TypeForm::Constant { t: Type::Unit }
                };

                if arg_name.is_some() {
                    env.insert(
                        arg_name.as_ref().unwrap().to_string(),
                        Scheme {
                            vars: vec![],
                            t: param_t.to_owned(),
                        },
                    );
                }

                let (body_e, body_t) = self.infer_j(env, body)?;

                let lambda_t = TypeForm::Arrow {
                    arg: param_t.into(),
                    ret: body_t.into(),
                };

                Ok((
                    Expr::LambdaFunction {
                        t: Some(lambda_t.to_owned()),
                        arg_name: arg_name.to_owned(),
                        body: body_e.into(),
                    },
                    lambda_t,
                ))
            }
            Expr::LetBinding {
                t: _,
                name,
                value,
                body,
            } => {
                let var_t = self.fresh_var();

                env.insert(
                    name.to_owned(),
                    Scheme {
                        vars: vec![],
                        t: var_t.to_owned(),
                    },
                );

                let (value_e, value_t) = self.infer_j(env, value)?;

                self.unify(&var_t, &value_t)?;
                let value_scheme = self.generalize(env, &value_t);

                env.insert(name.to_owned(), value_scheme);

                let (body_e, body_t) = self.infer_j(env, body)?;

                Ok((
                    Expr::LetBinding {
                        t: Some(body_t.to_owned()),
                        name: name.to_owned(),
                        value: value_e.into(),
                        body: body_e.into(),
                    },
                    body_t,
                ))
            }

            // Expr::Record { t: _, fields } => {
            //     let mut fields_e = HashMap::new();
            //     let mut fields_t = HashMap::new();
            //
            //     for (label, expr) in fields {
            //         let (expr_e, expr_t) = self.infer_j(env, expr)?;
            //
            //         fields_e.insert(label.to_owned(), expr_e);
            //         fields_t.insert(label.to_owned(), expr_t);
            //     }
            //
            //     let record_t = TypeForm::Record { fields: fields_t };
            //
            //     Ok((
            //         Expr::Record {
            //             t: Some(record_t.to_owned()),
            //             fields: fields_e,
            //         },
            //         record_t,
            //     ))
            // }
            //
            // Expr::Field {
            //     t: _,
            //     label,
            //     record,
            // } => {
            //     let (value_e, value_t) = self.infer_j(env, record)?;
            //     let field_t = self.fresh_var();
            //
            //     let fields_t = HashMap::<String, TypeForm>::from_iter(iter![(
            //         label.to_owned(),
            //         field_t.to_owned()
            //     )]);
            //     let fields_e =
            //         HashMap::<String, Expr>::from_iter(iter![(label.to_owned(), value_e)]);
            //
            //     let record_t = TypeForm::Record { fields: fields_t };
            //
            //     self.unify(&value_t, &record_t)?;
            //
            //     let record_e = Expr::Record {
            //         t: Some(record_t.to_owned()),
            //         fields: fields_e,
            //     };
            //
            //     Ok((
            //         Expr::Field {
            //             t: Some(record_t),
            //             label: label.to_owned(),
            //             record: record_e.into(),
            //         },
            //         field_t,
            //     ))
            // }
            Expr::List { t: _, value } => {
                let elem_t = self.fresh_var();

                let mut items_e = vec![];

                for item in value {
                    let (item_e, item_t) = self.infer_j(env, item)?;

                    self.unify(&item_t, &elem_t)?;

                    items_e.push(item_e);
                }

                let list_t = TypeForm::List {
                    items: elem_t.into(),
                };

                Ok((
                    Expr::List {
                        t: Some(list_t.to_owned()),
                        value: items_e,
                    },
                    list_t,
                ))
            }
        }
    }

    pub fn infer(&mut self, ast: &Node) -> Result<Node, Box<dyn Error>> {
        let mut context: Env = HashMap::new();

        // context.insert(
        //     "if".to_string(),
        //     Scheme {
        //         vars: vec!["t0".to_string()],
        //         t: Type::Arrow {
        //             arg: Type::Constant {
        //                 t: TypeName::Boolean,
        //             }
        //             .into(),
        //             ret: Type::Arrow {
        //                 arg: Type::Variable {
        //                     name: "t0".to_string(),
        //                 }
        //                 .into(),
        //                 ret: Type::Arrow {
        //                     arg: Type::Variable {
        //                         name: "t0".to_string(),
        //                     }
        //                     .into(),
        //                     ret: Type::Variable {
        //                         name: "t0".to_string(),
        //                     }
        //                     .into(),
        //                 }
        //                 .into(),
        //             }
        //             .into(),
        //         },
        //     },
        // );
        // context.insert(
        //     "<=".to_string(),
        //     Scheme {
        //         vars: vec![],
        //         t: Type::Arrow {
        //             arg: Type::Constant {
        //                 t: TypeName::Number,
        //             }
        //             .into(),
        //             ret: Type::Arrow {
        //                 arg: Type::Constant {
        //                     t: TypeName::Number,
        //                 }
        //                 .into(),
        //                 ret: Type::Constant {
        //                     t: TypeName::Boolean,
        //                 }
        //                 .into(),
        //             }
        //             .into(),
        //         },
        //     },
        // );
        // context.insert(
        //     "+".to_string(),
        //     Scheme {
        //         vars: vec!["t0".to_string()],
        //         t: Type::Arrow {
        //             arg: Type::Variable {
        //                 name: "t0".to_string(),
        //             }
        //             .into(),
        //             ret: Type::Arrow {
        //                 arg: Type::Variable {
        //                     name: "t0".to_string(),
        //                 }
        //                 .into(),
        //                 ret: Type::Variable {
        //                     name: "t0".to_string(),
        //                 }
        //                 .into(),
        //             }
        //             .into(),
        //         },
        //     },
        // );
        // context.insert(
        //     "-".to_string(),
        //     Scheme {
        //         vars: vec!["t0".to_string()],
        //         t: Type::Arrow {
        //             arg: Type::Variable {
        //                 name: "t0".to_string(),
        //             }
        //             .into(),
        //             ret: Type::Arrow {
        //                 arg: Type::Variable {
        //                     name: "t0".to_string(),
        //                 }
        //                 .into(),
        //                 ret: Type::Variable {
        //                     name: "t0".to_string(),
        //                 }
        //                 .into(),
        //             }
        //             .into(),
        //         },
        //     },
        // );
        context.insert(
            "print".to_string(),
            Scheme {
                vars: vec![],
                t: TypeForm::Arrow {
                    arg: TypeForm::Constant { t: Type::String }.into(),
                    ret: TypeForm::Constant { t: Type::Unit }.into(),
                },
            },
        );

        // (let main (fn []
        //   (print "Hello, world!")))
        let expr = Expr::LetBinding {
            t: None,
            name: "main".to_string(),
            value: Expr::LambdaFunction {
                t: None,
                arg: None,
                body: Expr::FunctionCall {
                    t: None,
                    f: Expr::Variable {
                        t: None,
                        name: "print".to_string(),
                    }
                    .into(),
                    arg: Expr::String {
                        t: None,
                        value: "Hello, world!".to_string(),
                    }
                    .into(),
                }
                .into(),
            }
            .into(),
            body: Expr::Unit { t: None }.into(),
        };

        let (result_e, result_t) = self.infer_j(&mut context, &expr)?;

        println!("\nRESULT E:\n{:#?}", result_e);
        println!("\nRESULT T:\n{:#?}", result_t);
        println!("\nRESULT UF:\n{:#?}", self.uf);
        println!("\nRESULT FRESH COUNTER:\n{:#?}", self.fresh_counter);

        Ok(ast.clone())
    }
}
