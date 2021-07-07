use std::sync::Arc;

use crate::storage::Storage;
use crate::structs::GetStruct;
use crate::{Result, Error, Program, make_query};
use crate::function::{Graph, InstructionType, JumpType, ObjectHandle};
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
    Compound(Constuctor, Vec<Type>),
}

impl Type {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            Type::U32 => false,
            Type::Bool => false,
            Type::String => false,
            Type::Never => false,
            Type::Null => false,
            Type::Struct(_) => false,
            Type::Compound(_, types) => types.iter().any(|t| t.depends_on(obj)),
        }
    }

    fn do_substitutions(&mut self, graph: &Graph) {
        match self {
            Type::Compound(_, types) => types.into_iter().for_each(|t| t.do_substitutions(graph)),
            _ => {},
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum TypeExpression {
    Type(Type),
    Placeholder(ObjectHandle),
    StructField(ObjectHandle, Vec<String>),
}

impl TypeExpression {
    fn depends_on(&self, obj: ObjectHandle) -> bool {
        match self {
            TypeExpression::Type(t) => t.depends_on(obj),
            TypeExpression::Placeholder(other) => obj == *other,
            TypeExpression::StructField(other, _) => obj == *other,
        }
    }

    async fn do_substitutions(&mut self, graph: &Graph, prog: Arc<Program>) -> Result<()>{
        match self {
            TypeExpression::Type(t) => {
                t.do_substitutions(graph);
                Ok(())
            },
            TypeExpression::Placeholder(obj) => {
                if let Some(ref sub) = graph.object(*obj).t {
                    *self = TypeExpression::Type(sub.clone());
                }
                Ok(())
            },
            TypeExpression::StructField(first_parent, field_spec) => {
                if let Some(first_parent_type) = graph.object(*first_parent).t.clone() {
                    let mut parent_type = first_parent_type;

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
            }
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
    fn add_eqn(&mut self, lhs: TypeExpression, rhs: TypeExpression) {
        self.equations.push(TypeEquation{lhs, rhs});
    }

    pub(crate) async fn add_types(graph: &mut Graph, prog: Arc<Program>, return_type: Type, return_object: ObjectHandle) -> Result<()> {
        let mut is = InferenceSystem{equations: Vec::new()};
        is.add_eqn(TypeExpression::Placeholder(return_object), TypeExpression::Type(return_type.clone()));
        for block_handle in graph.blocks() {
            let block = graph.block(block_handle);
            for inst in &block.instructions {
                match inst.t {
                    InstructionType::Eq{dest, lhs, rhs} => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::Bool));
                        is.add_eqn(TypeExpression::Placeholder(lhs), TypeExpression::Placeholder(rhs));
                    },
                    InstructionType::StoreU32(dest, _) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::U32));
                    },
                    InstructionType::StoreString(dest, _) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::String));
                    },
                    InstructionType::StoreNull(dest) => {
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::Null));
                    },
                    InstructionType::SetType(ref obj, ref t) => {
                        is.add_eqn(TypeExpression::Placeholder(*obj), TypeExpression::Type(t.clone()));
                    },
                    InstructionType::Add{dest, lhs, rhs} => {
                        is.add_eqn(TypeExpression::Placeholder(lhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(rhs), TypeExpression::Type(Type::U32));
                        is.add_eqn(TypeExpression::Placeholder(dest), TypeExpression::Type(Type::U32));
                    },
                    InstructionType::Copy{src, dst} => {
                        is.add_eqn(TypeExpression::Placeholder(src), TypeExpression::Placeholder(dst));
                    },
                    InstructionType::Call{ref dst, callee: _, ref signature, ref args} => {
                        is.add_eqn(TypeExpression::Placeholder(*dst), TypeExpression::Type(signature.return_type.clone()));
                        for (arg, sig) in args.iter().zip(&signature.args) {
                            is.add_eqn(TypeExpression::Placeholder(*arg), TypeExpression::Type(sig.1.clone()));
                        }
                    },
                    InstructionType::Return{src} => {
                        is.add_eqn(TypeExpression::Placeholder(src), TypeExpression::Type(return_type.clone()));
                    },
                    InstructionType::StructFieldAssignment{ref parent_object, ref field_spec, ref value} => {
                        is.add_eqn(TypeExpression::Placeholder(*value), TypeExpression::StructField(*parent_object, field_spec.clone()));
                    },
                    InstructionType::StructFieldAccess{ref parent_object, ref field, ref dest} => {
                        is.add_eqn(TypeExpression::Placeholder(*dest), TypeExpression::StructField(*parent_object, vec![field.clone()]));
                    },
                }
            }

            match &block.jump {
                None => {},
                Some(j) => {
                    match j.t {
                        JumpType::Return => {},
                        JumpType::Cond{condition, ..} => {
                            is.add_eqn(TypeExpression::Placeholder(condition), TypeExpression::Type(Type::Bool));
                        },
                        JumpType::Uncond(..) => {},
                    }
                }
            }
        }
        is.results(graph, prog).await?;
        Ok(())
    }

    async fn results(&mut self, graph: &mut Graph, prog: Arc<Program>) -> Result<()> {
        loop {
            self.delete_tautologies();
            self.decompose();
            self.check_conflict()?;
            self.swap();
            self.eliminate(graph, prog.clone()).await?;
            self.check()?;

            if !self.can_progress() {
                break;
            }
        }
        Self::fill_in_nevers(graph);
        Ok(())
    }

    fn fill_in_nevers(graph: &mut Graph) {
        for handle in graph.never_objects.clone() {
            let obj = graph.object_mut(handle);
            if obj.t.is_none() {
                obj.t = Some(Type::Never);
            } 
        }
    } 

    fn delete_tautologies(&mut self) {
        self.equations.retain(|e| e.lhs != e.rhs);
    }

    fn decompose(&mut self) {
        let mut new_equations = Vec::new();
        let mut to_remove = Vec::new();
        for (idx, eqn) in self.equations.iter().enumerate() {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Type(Type::Compound(c, args)), TypeExpression::Type(Type::Compound(c2, args2))) => {
                    if c == c2 && args.len() == args2.len() {
                        to_remove.push(idx);
                        for (a1, a2) in args.iter().zip(args2.iter()) {
                            new_equations.push(TypeEquation{
                                lhs: TypeExpression::Type(a1.clone()),
                                rhs: TypeExpression::Type(a2.clone()),
                            });
                        }
                    }
                },
                _ => {}
            }
        }

        for idx in to_remove.into_iter().rev() {
            self.equations.swap_remove(idx);
        }

        self.equations.append(&mut new_equations);
    }

    fn check_conflict(&self) -> Result<()> {
        for eqn in &self.equations {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Type(Type::Compound(c, args)), TypeExpression::Type(Type::Compound(c2, args2))) => {
                    if c != c2 || args.len() != args2.len() {
                        return Err(Error::new("Type error"));
                    }
                },
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

    async fn eliminate(&mut self, graph: &mut Graph, prog: Arc<Program>) -> Result<()>{
        let mut to_remove = Vec::new();
        let mut objects_already_removed = Storage::new();
        for (idx, eqn) in self.equations.iter().enumerate() {
            match (&eqn.lhs, &eqn.rhs) {
                (TypeExpression::Placeholder(_), TypeExpression::Placeholder(_)) => {},
                (TypeExpression::Placeholder(obj), TypeExpression::Type(rhs)) => {
                    if !rhs.depends_on(*obj) {
                        if let None = objects_already_removed.get(obj) {
                            *objects_already_removed.get_mut(obj) = Some(());
                            to_remove.push(idx);
                            graph.object_mut(*obj).t = Some(rhs.clone());
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
            eqn.lhs.do_substitutions(graph, prog.clone()).await?;
            eqn.rhs.do_substitutions(graph, prog.clone()).await?;
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

