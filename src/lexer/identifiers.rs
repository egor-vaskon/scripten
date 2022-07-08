use std::collections::HashMap;

pub struct IdentId(u32);

impl From<u32> for IdentId {
    fn from(id: u32) -> Self {
        IdentId(id)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Identifiers {
    registry: HashMap<String, u32>
}

impl<'a> Identifiers {
    pub fn new() -> Identifiers {
        Identifiers {
            registry: HashMap::new()
        }
    }

    #[cfg(test)]
    pub fn with_idents(idents: impl IntoIterator<Item = &'a str>) -> Identifiers {
        let idents: HashMap<String, u32> = idents
            .into_iter()
            .map(str::to_owned)
            .enumerate()
            .map(|(usize, ident)| (ident, usize as u32 + 1))
            .collect();

        Identifiers {
            registry: idents
        }
    }

    fn add(&mut self, identifier: &str) -> IdentId {
        if !self.registry.contains_key(identifier) {
            let id = self.registry.len() as u32 + 1; //id 0 is reserved
            self.registry.insert(identifier.to_owned(), id);
            id.into()
        } else {
            self.registry[identifier].into()
        }
    }
}