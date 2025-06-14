pub enum MaybeRef<'a, T> {
    Ref(&'a T),
    MutRef(&'a mut T),
    Owned(T),
}

impl<'a, T> MaybeRef<'a, T> {
    pub fn unwrap_owned(self) -> Result<T, MaybeRef<'a, T>> {
        match self {
            MaybeRef::Owned(t) => Ok(t),
            v => Err(v),
        }
    }

    pub fn unwrap_ref(self) -> Result<&'a T, MaybeRef<'a, T>> {
        match self {
            MaybeRef::Ref(t) => Ok(t),
            v => Err(v),
        }
    }

    pub fn unwrap_mut_ref(self) -> Result<&'a mut T, MaybeRef<'a, T>> {
        match self {
            MaybeRef::MutRef(t) => Ok(t),
            v => Err(v),
        }
    }

    pub fn get<'b>(&'b self) -> &'b T
    where
        'a: 'b,
    {
        match self {
            MaybeRef::Ref(t) => t,
            MaybeRef::MutRef(t) => t,
            MaybeRef::Owned(t) => t,
        }
    }

    pub fn get_mut<'b>(&'b mut self) -> Option<&'b mut T>
    where
        'a: 'b,
    {
        match self {
            MaybeRef::Ref(_) => None,
            MaybeRef::MutRef(t) => Some(*t),
            MaybeRef::Owned(t) => Some(t),
        }
    }
}
