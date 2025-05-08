#[macro_export]
macro_rules! create_index_wrapper {
    ($struct_ty: ty, $indexed_field: ident, $value_ty: ty, $idx_ty: ident) => {
        #[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
        #[repr(transparent)]
        pub struct $idx_ty(pub u32);

        impl std::ops::Index<$idx_ty> for $struct_ty {
            type Output = $value_ty;

            fn index(&self, index: $idx_ty) -> &Self::Output {
                &self.$indexed_field[index.0 as usize]
            }
        }
    };
}

#[macro_export]
macro_rules! create_index_wrapper_mut {
    ($struct_ty: ty, $indexed_field: ident, $value_ty: ty, $idx_ty: ident) => {
        $crate::create_index_wrapper!($struct_ty, $indexed_field, $value_ty, $idx_ty);
        
        impl std::ops::IndexMut<$idx_ty> for $struct_ty {
            fn index_mut(&mut self, index: $idx_ty) -> &mut Self::Output {
                &mut self.$indexed_field[index.0 as usize]
            }
        }
    };
}

// Copied from Rust's standard library
#[macro_export]
macro_rules! cfg_match {
    ({ $($tt:tt)* }) => {{
        cfg_match! { $($tt)* }
    }};

    (_ => { $($output:tt)* }) => {
        $($output)*
    };

    (
        $cfg:meta => $output:tt
        $($( $rest:tt )+)?
    ) => {
        #[cfg($cfg)]
        cfg_match! { _ => $output }
        $(
            #[cfg(not($cfg))]
            cfg_match! { $($rest)+ }
        )?
    };
}
