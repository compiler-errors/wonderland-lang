use crate::{
    parser::{ast::*, ast_visitor::AstAdapter},
    util::{Context, FileId, FileRegistry, PError, PResult, Visit},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type SharedModule = Rc<RefCell<MappedModule>>;

#[derive(Debug)]
pub struct ModuleMap {
    modules: HashMap<FileId, SharedModule>,
    pub top: SharedModule,
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
                    return perror!(
                        "Expected `{}` to be a module, instead it is a symbol!",
                        mod_path.join("::")
                    );
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
            let module = (*module).borrow();

            if !module.children.contains_key(child_name) {
                return perror!("No submodule with the name `{}`", child_name);
            }

            if let ModuleItem::Submodule(submodule) = &module.children[child_name] {
                submodule.clone()
            } else {
                return perror!(
                    "Expected `{}` to be a module, instead it is a symbol!",
                    mod_path.join("::")
                );
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

// Dummy impl to satisfy the visitor derive...
// TODO: Make a #[SkipVisit] or something
impl<T> Visit<T> for Rc<RefCell<MappedModule>> {}

impl MappedModule {
    fn new_shared() -> SharedModule {
        Rc::new(RefCell::new(MappedModule {
            children: HashMap::new(),
        }))
    }

    pub fn get_child(&self, item: &str, path: &[String]) -> PResult<ModuleItem> {
        self.children
            .get(item)
            .ok_or_else(|| {
                if path.is_empty() {
                    PError::new(format!("No such item `{}` in current module!", item,))
                } else {
                    PError::new(format!(
                        "No such item `{}` in module `{}`",
                        item,
                        path.join("::")
                    ))
                }
            })
            .cloned()
    }

    pub fn top_level_symbols(&self) -> Vec<(String, FileId)> {
        self.children
            .iter()
            .map(|(name, item)| {
                if let ModuleItem::Symbol(id) = item {
                    Some((name.clone(), *id))
                } else {
                    None
                }
            })
            .filter_map(|x| x)
            .collect()
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
    fn enter_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        let id = m.id;
        let symbols: Vec<_> = (m.functions.keys())
            .chain(m.traits.keys())
            .chain(m.objects.keys())
            .chain(m.globals.keys())
            .chain(m.enums.keys())
            .collect();

        let path = FileRegistry::mod_path(m.id);
        let module = self.mod_map.get_or_create_module(&path)?;

        // Mutably borrow the module so we can mess with it!
        {
            let children = &mut (*module).borrow_mut().children;

            for name in symbols {
                if children.contains_key(name) {
                    return perror!("Duplicated symbol `{}`", name);
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
    this_id: FileId,
    this_children: &mut HashMap<String, ModuleItem>,
    module: &[String],
    name: &str,
    child: ModuleItem,
) -> PResult<bool> {
    if this_children.contains_key(name) {
        if this_children[name] != child {
            debug!("{:#?} VS. {:#?}", this_children[name], child);

            return perror!(
                "Cannot import symbol `{}` from module `{}` when it already exists in the module \
                 `{}`!",
                name,
                module.join("::"),
                FileRegistry::mod_path(this_id).join("::")
            );
        }
        Ok(false)
    } else {
        this_children.insert(name.into(), child.clone());
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
    fn enter_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        for u in &m.pub_uses {
            match u {
                AstUse::Use(module, item) => {
                    let use_module_ref = self.mod_map.get_module(module)?;
                    let child = (*use_module_ref).borrow().get_child(item, module)?;

                    let this_module_ref = self.mod_map.get_module(&FileRegistry::mod_path(m.id))?;
                    let mut this_module = (*this_module_ref).borrow_mut();

                    let res = import(m.id, &mut this_module.children, module, item, child);

                    match res {
                        Ok(true) => self.modified = true,
                        Err(e) => self.err = Some(e),
                        _ => {},
                    }
                },
                AstUse::UseAll(module) => {
                    let use_module_ref = self.mod_map.get_module(module)?;
                    let use_module = (*use_module_ref).borrow();

                    for (name, child) in &use_module.children {
                        let this_module_ref =
                            self.mod_map.get_module(&FileRegistry::mod_path(m.id))?;
                        let mut this_module = (*this_module_ref).borrow_mut();

                        let res =
                            import(m.id, &mut this_module.children, module, name, child.clone());

                        match res {
                            Ok(true) => self.modified = true,
                            Err(e) => self.err = Some(e),
                            _ => {},
                        }
                    }
                },
            }
        }

        Ok(m)
    }
}

pub struct AnalyzeUses<'a> {
    mod_map: &'a ModuleMap,
    current_mod: Option<SharedModule>,
    current_mod_name: Option<String>,
    pub modules: HashMap<FileId, SharedModule>,
}

impl<'a> AnalyzeUses<'a> {
    pub fn new(mod_map: &'a ModuleMap) -> AnalyzeUses<'a> {
        AnalyzeUses {
            mod_map,
            current_mod: None,
            current_mod_name: None,
            modules: HashMap::new(),
        }
    }
}

impl<'a> AstAdapter for AnalyzeUses<'a> {
    fn enter_ast_module(&mut self, mut m: AstModule) -> PResult<AstModule> {
        let std_path = vec!["std".into()];

        if FileRegistry::mod_path(m.id) != std_path {
            debug!("Using std in {}", FileRegistry::mod_path(m.id).join("::"));
            m.uses.push(AstUse::UseAll(std_path));
        } else {
            debug!(
                "Not using std in {}",
                FileRegistry::mod_path(m.id).join("::")
            );
        }

        if self.current_mod.is_some() {
            return perror!("ICE: Previous module wasn't cleaned up!");
        }

        let current_mod_ref = self.mod_map.get_module_clone(m.id);

        // Explicitly denote the borrow of current_mod_ref.
        {
            // I know I could process these in an enter* function,
            // but I want to make sure they all happen at the beginning.
            // That's also already determined to happen, but whatever.
            for u in &m.uses {
                match u {
                    AstUse::Use(module, item) => {
                        let use_module_ref = self.mod_map.get_module(module)?;
                        let child = (*use_module_ref).borrow().get_child(item, module)?;

                        let mut current_mod = (*current_mod_ref).borrow_mut();

                        import(m.id, &mut current_mod.children, module, item, child)?;
                    },
                    AstUse::UseAll(module) => {
                        let use_module_ref = self.mod_map.get_module(module)?;
                        let use_module = (*use_module_ref).borrow();

                        for (name, child) in &use_module.children {
                            let mut current_mod_ref = (*current_mod_ref).borrow_mut();

                            import(
                                m.id,
                                &mut current_mod_ref.children,
                                module,
                                name,
                                child.clone(),
                            )?;
                        }
                    },
                }
            }

            let mut current_mod_ref = (*current_mod_ref).borrow_mut();

            // Import everything from the top-level module, SHADOWED!!
            // In other words, if something explicitly imported is
            // shadowing that import then ignore it.
            for (item, child) in &(*self.mod_map.top).borrow().children {
                let _res = import(
                    m.id,
                    &mut current_mod_ref.children,
                    &[],
                    item,
                    child.clone(),
                );
                // EXPLICITLY ignore result.
            }
        }

        self.current_mod = Some(current_mod_ref);
        self.current_mod_name = Some(FileRegistry::mod_path(m.id).join("::"));
        Ok(m)
    }

    fn enter_module_ref(&mut self, m: ModuleRef) -> PResult<ModuleRef> {
        match m {
            ModuleRef::Denormalized(path) => {
                let (item, mod_path) = path.split_last().unwrap();
                let module_ref = get_module(self.current_mod.as_ref().unwrap(), mod_path)?;
                let module = (*module_ref).borrow();

                if let ModuleItem::Symbol(file) =
                    module.get_child(item, mod_path).with_comment(|| {
                        format!("In module: `{}`", self.current_mod_name.as_ref().unwrap())
                    })?
                {
                    Ok(ModuleRef::Normalized(file, path.last().unwrap().clone()))
                } else {
                    perror!("Reference {} is not a symbol!", path.join("::"))
                }
            },
            n @ ModuleRef::Normalized(..) => Ok(n),
        }
    }

    fn exit_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        let s = std::mem::take(&mut self.current_mod);
        self.modules.insert(m.id, s.unwrap()).expect_none("Yikes");

        Ok(m)
    }
}
