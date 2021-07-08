use std::sync::Arc;

use crate::storage::Storage;
use crate::structs::GetStruct;
use crate::{Result, Error, Program, make_query};
use crate::function::ObjectHandle;
use crate::itemise::ItemPath;


// Type System
#[derive(Clone, PartialEq, Debug, Eq)]
pub(crate) enum Constuctor {}

#[derive(Clone, PartialEq, Debug, Eq)]
pub(crate) enum Type {
    U32,
    Bool,
    String,
    Never,
    Null,
    Struct(ItemPath),
}

#[derive(Clone, PartialEq, Debug)]
enum TypeOrExpression {
    Type(Type),
    Expression(TypeExpression),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TypeExpression {
    Type(Type),
    Placeholder(ObjectHandle),
    StructField(ObjectHandle, Vec<String>),
}

impl TypeExpression {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            TypeExpression::Type(_) => false,
            TypeExpression::Placeholder(other) => obj == *other,
            TypeExpression::StructField(other, _) => obj == *other,
        }
    }

    async fn do_substitutions(&mut self, types: &Storage<ObjectHandle, Type>, prog: Arc<Program>) -> Result<()> {
        match self {
            TypeExpression::Type(_) => {
                Ok(())
            },
            TypeExpression::Placeholder(obj) => {
                if let Some(sub) = types.get(obj) {
                    *self = TypeExpression::Type(sub.clone());
                }
                Ok(())
            },
            TypeExpression::StructField(first_parent, field_spec) => {
                if let Some(first_parent_type) = types.get(first_parent) {
                    let mut parent_type = first_parent_type.clone();

                    for field in field_spec {
                        if let Type::Struct(ref item_path) = parent_type {
                            let s = make_query!(prog, GetStruct{path: item_path.clone()}).await?;

                            parent_type = match s.get_member(field) {
                                Some(t) => t.clone(),
                                None => return Err(Error::new("Struct has no such field"))
                            };

                        } else {
                            return Err(Error::new("Field access on non struct type"));
                        }
                    }
                    *self = TypeExpression::Type(parent_type);
                }

                Ok(())
            },
        }
    }
}

#[derive(Debug)]
struct TypeEquation {
    lhs: TypeExpression,
    rhs: TypeExpression,
}

#[derive(Debug)]
pub(crate) struct InferenceSystem {
    equations: Vec<TypeEquation>,
}

impl InferenceSystem {
    pub(crate) fn new() -> Self {
        Self {
            equations: Vec::new()
        }
    }

    pub(crate) fn add_eqn(&mut self, lhs: TypeExpression, rhs: TypeExpression) {
        self.equations.push(TypeEquation{lhs, rhs});
    }

    pub(crate) async fn results(&mut self, never_objects: &[ObjectHandle], prog: Arc<Program>) -> Result<Storage<ObjectHandle, Type>> {
        let mut types = Storage::new();
        loop {
            self.delete_tautologies();
            self.decompose();
            self.check_conflict()?;
            self.swap();
            self.eliminate(&mut types, prog.clone()).await?;
            self.check()?;

            if !self.can_progress() {
                break;
            }
        }
        Self::fill_in_nevers(never_objects, &mut types);

        Ok(types)
    }

    fn fill_in_nevers(never_objects: &[ObjectHandle], types: &mut Storage<ObjectHandle, Type>) {
        for handle in never_objects {
            let obj = types.get_mut(handle);
            if obj.is_none() {
                *obj = Some(Type::Never);
            } 
        }
    } 

    fn delete_tautologies(&mut self) {
        self.equations.retain(|e| e.lhs != e.rhs);
    }

    fn decompose(&mut self) {

    }

    fn check_conflict(&self) -> Result<()> {
        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Type(a), TypeExpression::Type(b)) => {
                    if a != b {
                        return Err(Error::new("Type error2"));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn swap(&mut self) {
        for eqn in &mut self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (_, TypeExpression::Placeholder(_)) => {
                    std::mem::swap(&mut eqn.lhs, &mut eqn.rhs);
                },
                _ => {}
            }
        }
    }

    async fn eliminate(&mut self, types: &mut Storage<ObjectHandle, Type>, prog: Arc<Program>) -> Result<()>{
        let mut to_remove = Vec::new();
        let mut objects_already_removed = Storage::new();
        for (idx, eqn) in self.equations.iter().enumerate() {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Placeholder(_), TypeExpression::Placeholder(_)) => {},
                (TypeExpression::Placeholder(obj), TypeExpression::Type(rhs)) => {
                    if !eqn.rhs.depends_on(*obj) {
                        if let None = objects_already_removed.get(obj) {
                            *objects_already_removed.get_mut(obj) = Some(());
                            to_remove.push(idx);
                            *types.get_mut(obj) = Some(rhs.clone());
                        }
                    }
                },
                _ => {}
            }
        }

        for idx in to_remove.into_iter().rev() {
            self.equations.swap_remove(idx);
        }

        for eqn in &mut self.equations {
            eqn.lhs.do_substitutions(types, prog.clone()).await?;
            eqn.rhs.do_substitutions(types, prog.clone()).await?;
        }

        Ok(())
    }

    fn check(&self) -> Result<()> {
        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Placeholder(obj), rhs) => {
                    if rhs.depends_on(*obj) {
                        return Err(Error::new("Self referential type"));
                    }
                },
                _ => {}
            }
        }
        Ok(())
    }

    fn can_progress(&self) -> bool {
        if self.equations.is_empty() {
            return false;
        }

        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Placeholder(_), TypeExpression::Placeholder(_)) => {},
                (TypeExpression::Placeholder(_), TypeExpression::StructField(_, _)) => {},
                _ => return true,
            }
        }
        return false;
    }
}

