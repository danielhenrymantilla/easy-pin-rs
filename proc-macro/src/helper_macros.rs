#[cfg(any())]
macro_rules! mk_debug {() => (mk_debug!($)); ($dol:tt) => (
    #[derive(Default)]
    struct DbgString /* = */ (
        String,
    );

    impl Drop for DbgString {
        fn drop (self: &'_ mut Self)
        {
            if ops::Not::not(self.0.is_empty()) {
                panic!("\n{}", self.0);
            }
        }
    }

    let mut debug = DbgString::default();

    macro_rules! debug {(
        $dol expr:expr
    ) => (
        match $dol expr { expr => {
            ::std::fmt::Write::write_fmt(
                &mut debug.0,
                format_args!(
                    concat!(
                        "[", file!(), ":", line!(), ":", column!(), "] ",
                        "{} = {:#?}\n",
                    ),
                    stringify!($dol expr),
                    expr,
                ),
            ).unwrap();
            expr
    }})}
)}

macro_rules! mk_render {
    ($ret:ident) => (
        mk_render!($ret $)
    );
    ($ret:ident $dol:tt) => (
        let mut $ret = TokenStream::new();

        macro_rules! render_spanned {($dol span:expr =>
            $dol($dol tt:tt)*
        ) => (
            $ret.extend(TokenStream::from(quote_spanned! { $dol span =>
                $dol($dol tt)*
            }))
        )}

        macro_rules! render {(
            $dol($dol tt:tt)*
        ) => (
            render_spanned!(::proc_macro2::Span::call_site()=>
                $dol($dol tt)*
            )
        )}
    );
}
