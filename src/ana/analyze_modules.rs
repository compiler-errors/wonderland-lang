use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{FileId, PError};
use crate::util::{FileRegistry, IntoError, PResult};
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

type SharedModule = Rc<RefCell<MappedModule>>;

#[derive(Debug)]
pub struct ModuleMap {
    modules: HashMap<FileId, SharedModule>,
    top: SharedModule,
}

impl ModuleMap {
    pub fn new() -> ModuleMap {
        ModuleMap {
            modules: HashMap::new(),
            top: MappedModule::new_shared(),
        }
    }

    fn get_module_clone(&self, file: FileId) -> SharedModule {
        let module = self.modules[&file].clone();
        let module_clone = (*module).borrow().clone();

        Rc::new(RefCell::new(module_clone))
    }

    fn get_or_create_module(&mut self, mod_path: &[String]) -> PResult<SharedModule> {
        let mut module = self.top.clone();

        for child_name in mod_path {
            let next_module = {
                let mut module_mut = (*module).borrow_mut();

                if !module_mut.children.contains_key(child_name) {
                    module_mut.children.insert(
                        child_name.clone(),
                        ModuleItem::Submodule(MappedModule::new_shared()),
                    );
                }

                if let ModuleItem::Submodule(submodule) = &module_mut.children[child_name] {
                    submodule.clone()
                } else {
                    return PResult::error(format!(
                        "Expected `{}` to be a module, instead it is a symbol!",
                        mod_path.join("::")
                    ));
                }
            };

            module = next_module;
        }

        Ok(module)
    }

    fn get_module(&self, mod_path: &[String]) -> PResult<SharedModule> {
        get_module(&self.top, mod_path)
    }
}

fn get_module(module: &SharedModule, mod_path: &[String]) -> PResult<SharedModule> {
    let mut module = module.clone();

    for child_name in mod_path {
        let next_module = {
            let mut module_mut = (*module).borrow_mut();

            if !module_mut.children.contains_key(child_name) {
                return PResult::error(format!("No submodule with the name `{}`", child_name));
            }

            if let ModuleItem::Submodule(submodule) = &module_mut.children[child_name] {
                submodule.clone()
            } else {
                return PResult::error(format!(
                    "Expected `{}` to be a module, instead it is a symbol!",
                    mod_path.join("::")
                ));
            }
        };

        module = next_module;
    }

    Ok(module)
}

#[derive(Debug, Clone)]
pub struct MappedModule {
    children: HashMap<String, ModuleItem>,
}

impl MappedModule {
    fn new_shared() -> SharedModule {
        Rc::new(RefCell::new(MappedModule {
            children: HashMap::new(),
        }))
    }

    fn get_child(&self, item: &str, path: &[String]) -> PResult<ModuleItem> {
        self.children
            .get(item)
            .ok_or_else(|| {
                PError::new(format!(
                    "No such item `{}` in module `{}`",
                    item,
                    path.join("::")
                ))
            })
            .map(Clone::clone)
    }
}

#[derive(Clone, Debug)]
pub enum ModuleItem {
    Symbol(FileId),
    Submodule(SharedModule),
}

impl PartialEq for ModuleItem {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ModuleItem::Symbol(f1), ModuleItem::Symbol(f2)) => f1 == f2,
            (ModuleItem::Submodule(m1), ModuleItem::Submodule(m2)) => m1.as_ptr() == m2.as_ptr(),
            _ => false,
        }
    }
}

pub struct AnalyzeModules<'a> {
    mod_map: &'a mut ModuleMap,
}

impl<'a> AnalyzeModules<'a> {
    pub fn new(mod_map: &'a mut ModuleMap) -> AnalyzeModules<'a> {
        AnalyzeModules { mod_map }
    }
}

impl<'a> AstAdapter for AnalyzeModules<'a> {
    fn enter_module(&mut self, m: AstModule) -> PResult<AstModule> {
        let id = m.id;
        let mut symbols: Vec<_> = (m.functions.keys())
            .chain(m.traits.keys())
            .chain(m.objects.keys())
            .collect();

        let path = FileRegistry::mod_path(m.id)?;
        let module = self.mod_map.get_or_create_module(&path)?;

        // Mutably borrow the module so we can mess with it!
        {
            let children = &mut (*module).borrow_mut().children;

            for name in symbols {
                if children.contains_key(name) {
                    return PResult::error(format!("Duplicated symbol `{}`", name));
                }

                children.insert(name.clone(), ModuleItem::Symbol(id));
            }
        }

        self.mod_map.modules.insert(id, module);
        Ok(m)
    }
}

/// I know the signature for this function is kinda icky...
/// But I wanted to be able to share this between PubUse and Use.
pub fn import(
    mod_map: &ModuleMap,
    this_id: FileId,
    this_children: &mut HashMap<String, ModuleItem>,
    module: &[String],
    item: &str,
) -> PResult<bool> {
    let use_module_ref = mod_map.get_module(module)?;
    let use_module = (*use_module_ref).borrow();
    let child = use_module.get_child(item, module)?;

    if this_children.contains_key(item) {
        if this_children[item] != child {
            return PResult::error(format!(
                "Cannot import symbol `{}` from module `{}` when it already exists in the module `{}`!",
                item,
                module.join("::"),
                FileRegistry::mod_path(this_id)?.join("::")
            ));
        }
        Ok(false)
    } else {
        this_children.insert(item.into(), child.clone());
        Ok(true)
    }
}

pub struct AnalyzePubUses<'a> {
    mod_map: &'a mut ModuleMap,
    pub modified: bool,
    pub err: Option<PError>,
}

impl<'a> AnalyzePubUses<'a> {
    pub fn new(mod_map: &'a mut ModuleMap) -> AnalyzePubUses<'a> {
        AnalyzePubUses {
            mod_map,
            modified: false,
            err: None,
        }
    }
}

impl<'a> AstAdapter for AnalyzePubUses<'a> {
    fn enter_module(&mut self, m: AstModule) -> PResult<AstModule> {
        let this_module_ref = self.mod_map.get_module(&FileRegistry::mod_path(m.id)?)?;
        let mut this_module = (*this_module_ref).borrow_mut();

        for u in &m.pub_uses {
            match u {
                AstUse::Use(module, item) => {
                    let res = import(&self.mod_map, m.id, &mut this_module.children, module, item);

                    match res {
                        Ok(true) => self.modified = true,
                        Err(e) => self.err = Some(e),
                        _ => {}
                    }
                }
                AstUse::UseAll(module) => {
                    let use_module_ref = self.mod_map.get_module(module)?;
                    let use_module = (*use_module_ref).borrow();

                    for item in use_module.children.keys() {
                        let res =
                            import(&self.mod_map, m.id, &mut this_module.children, module, item);

                        match res {
                            Ok(true) => self.modified = true,
                            Err(e) => self.err = Some(e),
                            _ => {}
                        }
                    }
                }
            }
        }

        Ok(m)
    }
}

pub struct AnalyzeUses<'a> {
    mod_map: &'a ModuleMap,
    current_mod: Option<SharedModule>,
}

impl<'a> AnalyzeUses<'a> {
    pub fn new(mod_map: &'a ModuleMap) -> AnalyzeUses<'a> {
        AnalyzeUses {
            mod_map,
            current_mod: None,
        }
    }
}

impl<'a> AstAdapter for AnalyzeUses<'a> {
    fn enter_module(&mut self, m: AstModule) -> PResult<AstModule> {
        if self.current_mod.is_some() {
            return PResult::error(format!("Previous module wasn't cleaned up!"));
        }

        let current_mod_ref = self.mod_map.get_module_clone(m.id);

        // Explicitly denote the borrow of current_mod_ref.
        {
            let mut current_mod = (*current_mod_ref).borrow_mut();

            // I know I could process these in an enter* function,
            // but I want to make sure they all happen at the beginning.
            // That's also already determined to happen, but whatever.
            for u in &m.uses {
                match u {
                    AstUse::Use(module, item) => {
                        import(&self.mod_map, m.id, &mut current_mod.children, module, item)?;
                    }
                    AstUse::UseAll(module) => {
                        let use_module_ref = self.mod_map.get_module(module)?;
                        let use_module = (*use_module_ref).borrow();

                        for item in use_module.children.keys() {
                            import(&self.mod_map, m.id, &mut current_mod.children, module, item)?;
                        }
                    }
                }
            }

            // Import everything from the top-level module, SHADOWED!!
            // In other words, if something explicitly imported is
            // shadowing that import then ignore it.
            for item in (*self.mod_map.top).borrow().children.keys() {
                let _res = import(
                    &self.mod_map,
                    m.id,
                    &mut current_mod.children,
                    &vec![],
                    item,
                );
                // EXPLICITLY ignore result.
            }
        }

        self.current_mod = Some(current_mod_ref);
        Ok(m)
    }

    fn enter_module_ref(&mut self, m: ModuleRef) -> PResult<ModuleRef> {
        match m {
            ModuleRef::Denormalized(path) => {
                let (item, mod_path) = path.split_last().unwrap();
                let module_ref = get_module(self.current_mod.as_ref().unwrap(), mod_path)?;
                let module = (*module_ref).borrow();

                if let ModuleItem::Symbol(file) = module.get_child(item, mod_path)? {
                    Ok(ModuleRef::Normalized(file, path.last().unwrap().clone()))
                } else {
                    PResult::error(format!("Reference {} is not a symbol!", path.join("::")))
                }
            }
            n @ ModuleRef::Normalized(..) => Ok(n),
        }
    }

    fn exit_module(&mut self, m: AstModule) -> PResult<AstModule> {
        self.current_mod = None;

        Ok(m)
    }
}
