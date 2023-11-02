macro_rules! deref {
    ($from:ty, $to:ty) => {
        impl std::ops::Deref for $from {
            type Target = $to;
            fn deref(&self) -> &$to {
                &self.0
            }
        }
    };
}
