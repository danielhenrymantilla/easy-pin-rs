#![allow(clippy::match_ref_pats)] // clearer borrowing semantics

use ::std::{*,
    collections::HashMap as Map,
    ops::Not,
};
extern crate proc_macro; use ::proc_macro::{
    TokenStream,
};
use ::proc_macro2::{
    Span,
};
use ::quote::{
    quote_spanned,
};
use ::syn::{self,
    DeriveInput,
    Ident,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    Visibility,
};

#[macro_use]
mod helper_macros;

macro_rules! error_spanned {($span:expr => $msg:expr) => ({
    let msg = syn::LitStr::new($msg, $span);
    return TokenStream::from(quote_spanned! { $span =>
        compile_error!(#msg);
    });
})}

#[cfg_attr(feature = "extra-traits",
    derive(Debug)
)]
#[derive(Default)]
struct DerivePinParams {
    drop: Option<Span>,
    unpin: Option<Span>,
}
impl Parse for DerivePinParams {
    fn parse (params: ParseStream<'_>) -> Result<Self, syn::Error>
    {
        let mut ret = Self::default();
        let idents = Punctuated::<Ident, syn::Token![,]>::
            parse_terminated(params)?
        ;
        for ident in idents {
            if ident == "Drop" {
                let prev_drop = ret.drop.replace(ident.span());
                if prev_drop.is_some() {
                    return Err(syn::Error::new(
                        ident.span(),
                        "Error, duplicated param `Drop`",
                    ));
                }
            } else if ident == "Unpin" {
                let prev_unpin = ret.unpin.replace(ident.span());
                if prev_unpin.is_some() {
                    return Err(syn::Error::new(
                        ident.span(),
                        "Error, duplicated param `Unpin`",
                    ));
                }
            } else {
                return Err(syn::Error::new(
                    ident.span(),
                    "Invalid parameter; expected `Drop` or `Unpin`",
                ));
            }
        }
        Ok(ret)
    }
}

#[cfg_attr(feature = "extra-traits",
    derive(Debug)
)]
struct SpecialField {
    pin_transitiveness: PinTransitiveness,
    vis: Visibility,
    ty: syn::Type,
}
#[cfg_attr(feature = "extra-traits",
    derive(Debug)
)]
enum PinTransitiveness {
    PinTransitive,
    Unpinned,
}
use PinTransitiveness::*;

impl SpecialField {
    fn collect_from<'fields> (
        fields: impl
            Iterator<Item = &'fields mut syn::Field> +
            iter::ExactSizeIterator +
        ,
    ) -> Result<
        Map<Ident, Self>,
        (Span, impl AsRef<str> + 'static),
    >
    {
        let mut ret = Map::with_capacity(fields.len());
        for (i, &mut syn::Field {
            attrs: ref mut field_attrs,
            ident: ref field_ident,
            ty: ref field_ty,
            ..
        })      in fields.enumerate()
        {
            let mut error_spanned = None;
            let mut had_attr = false;
            field_attrs.retain(|attr: &syn::Attribute| -> bool {
                macro_rules! ignore_attr {() => (return true)}
                if error_spanned.is_some() { ignore_attr!() }
                macro_rules! error_spanned {
                    ($span:expr =>
                        $msg:expr
                    ) => ({
                        error_spanned = Some(($span, $msg));
                        ignore_attr!()
                    });
                }

                // Ignore outer attributes (#![...])
                match &attr.style { //
                    | &syn::AttrStyle::Inner(_) => ignore_attr!(),
                    | &syn::AttrStyle::Outer => {},
                }

                // Ignore badly ill-formed attributes
                let attr_meta = if let Ok(attr_meta) = attr.parse_meta() {
                    attr_meta
                } else {
                    ignore_attr!();
                };

                // Treat only transitively_pinned / unpinned
                let pin_transitiveness = match &attr_meta { //
                    | &syn::Meta::Word(ref ident)
                    | &syn::Meta::NameValue(syn::MetaNameValue {
                        ref ident,
                        ..
                    })
                    | &syn::Meta::List(syn::MetaList {
                        ref ident,
                        ..
                    })
                    => if ident == "transitively_pinned" {
                        PinTransitive
                    } else if ident == "unpinned" {
                        Unpinned
                    } else {
                        ignore_attr!();
                    },
                };
                had_attr = true;
                // From now on, instead of ignoring, we error.

                let span = attr_meta.span();
                let visibility = match attr_meta { //
                    | syn::Meta::Word(_)
                    => Visibility::Inherited,

                    | syn::Meta::List(syn::MetaList {
                        nested,
                        ..
                    }) => {
                        let span = nested.span();
                        let mut nested = nested.into_iter();
                        if nested.len() != 1 {
                            error_spanned!(span =>
                                "Too many parameters, at most 1 expected"
                            );
                        }
                        match nested.next() { //
                            | Some(
                                syn::NestedMeta::Meta(
                                    syn::Meta::NameValue(
                                        syn::MetaNameValue {
                                            ref ident,
                                            lit: syn::Lit::Str(ref string_literal),
                                            ..
                                        }
                                    )
                                )
                            ) if *ident == "pub"
                            => match syn::parse_str::<Visibility>(&format!(
                                "pub ({})", string_literal.value(),
                            )) {//
                                | Ok(vis) => vis,
                                | _ => error_spanned!(string_literal.span() =>
                                    "expected visibility specifier"
                                ),
                            },

                            | Some(
                                syn::NestedMeta::Meta(
                                    syn::Meta::Word(ident)
                                )
                            ) => {
                                let span = ident.span();
                                if ident != "pub" {
                                    error_spanned!(span =>
                                        "expected `pub`"
                                    );
                                }
                                Visibility::Public(syn::VisPublic {
                                    pub_token: syn::token::Pub { span },
                                })
                            },

                            | Some(otherwise) => {
                                error_spanned!(otherwise.span() =>
                                    r#"Expected `pub = "..."`"#
                                );
                            },

                            | _ => error_spanned!(span =>
                                r#"Expected `pub = "..."`"#
                            ),
                        }
                    },

                    | _ => error_spanned!(attr_meta.span() =>
                        r#"Expected `pub = "..."`"#
                    ),
                };
                let ident =
                    field_ident
                        .clone()
                        .unwrap_or_else(|| Ident::new(
                            &i.to_string(),
                            field_ty.span(),
                        ))
                ;
                let prev = ret.insert(ident, SpecialField {
                    pin_transitiveness,
                    vis: visibility,
                    ty: field_ty.clone(),
                });
                if prev.is_some() {
                    error_spanned!(span => concat!(
                        "#[unpinned] / #[transitively_pinned] ",
                        "must be specified exactly once per field",
                    ));
                }
                false
            });
            if let Some((span, msg)) = error_spanned { //
                return Err((span, msg));
            }
            if had_attr.not() {
                let span =
                    field_ident
                        .as_ref()
                        .map(Ident::span)
                        .unwrap_or_else(|| field_ty.span())
                ;
                return Err((span, concat!(
                    "Missing #[unpinned] or #[transitively_pinned] attribute; ",
                    "when in doubt, use #[unpinned].",
                )));
            }
        }
        Ok(ret)
    }
}

#[proc_macro_attribute] pub
fn easy_pin (params: TokenStream, input: TokenStream) -> TokenStream
{
    mk_render!(ret);

    // attribute should apply on a struct
    let mut input: DeriveInput = syn::parse_macro_input!(input);
    let (impl_generics, ty_generics, where_clause) =
        input
            .generics
            .split_for_impl()
    ;
    let at_struct = {
        use syn::{Data, DataEnum, DataUnion};
        use syn::token::{Enum, Union};
        match &mut input.data { //
            | &mut Data::Struct(ref mut at_struct) => at_struct,

            | &mut Data::Enum(DataEnum {
                enum_token: Enum { span, .. },
                ..
            })
            | &mut Data::Union(DataUnion {
                union_token: Union { span, .. },
                ..
            }) => error_spanned!(span =>
                "#[easy_pin] only works on structs (currently)"
            ),
        }
    };
    let struct_name = &input.ident;

    // parse each field to identify those "special"
    // (i.e. marked #[transitively_pinned] / #[unpinned])
    // note: this does mutate the original fields to strip these attributes
    // to avoid Rust erroring on unknown attributes
    let special_fields = {
        match SpecialField::collect_from(at_struct.fields.iter_mut()) {//
            | Ok(special_fields) => special_fields,
            | Err((span, msg)) => error_spanned!(span => msg.as_ref()),
        }
    };

    // Now that our custom attributes have been parsed and stripped,
    // we may render the input struct, thus almost acting like a #[derive()]
    render! {
        #input
    }

    // Handle the Drop optional input parameter on the main proc_macro_attribute
    let params: DerivePinParams = syn::parse_macro_input!(params);
    if let Some(span) = params.drop {
        render_spanned! { span =>
            impl #impl_generics Drop
                for #struct_name #ty_generics
            #where_clause
            {
                #[inline]
                fn drop (self: &'_ mut Self)
                {
                    unsafe {
                        <Self as easy_pin::PinDrop>::drop_pinned(
                            easy_pin::core::pin::Pin::new_unchecked(self)
                        )
                    }
                }
            }
        }
    } else {
        // To avoid an unsound impl of `Drop` that does not use PinDrop,
        // let's add a dummy empty Drop that should conflict with any such impl
        render! {
            impl #impl_generics Drop
                for #struct_name #ty_generics
            #where_clause
            {
                #[inline]
                fn drop (self: &'_ mut Self)
                {}
            }
        }
    }

    // Add Unpin when pinned fields are Unpin
    if let Some(span) = params.unpin {
        let mut where_clause =
            where_clause
                .cloned()
                .unwrap_or_else(|| syn::WhereClause {
                    where_token: syn::token::Where {
                        span/*: Span::call_site()*/,
                    },
                    predicates: Punctuated::new(),
                })
        ;
        let unpin_trait: syn::Path = syn::parse_quote! {
            easy_pin::core::marker::Unpin
        };
        where_clause.predicates.extend(special_fields.values().filter_map(
            |field: &'_ SpecialField| -> Option<syn::WherePredicate>
            {
                match field {//
                    | SpecialField { pin_transitiveness: Unpinned, ..} => {
                        None
                    },
                    | SpecialField { ty: field_ty, .. } => {
                        Some(syn::parse_quote! {
                            #field_ty : #unpin_trait
                        })
                    },
                }
            }
        ));
        render_spanned! { span =>
            impl #impl_generics #unpin_trait
                for #struct_name #ty_generics
            #where_clause
            {}
        }
    }

    // Add Pin/Unpin projections
    special_fields.into_iter().for_each(|(ident, field)| match field {
        // Pin projection
        | SpecialField {
            pin_transitiveness: PinTransitive,
            vis,
            ty,
        } => {
            // & _
            let pinned_ident = Ident::new(
                &format!("pinned_{}", ident),
                ident.span(),
            );
            render_spanned! { ident.span() =>
                impl #impl_generics
                    #struct_name #ty_generics
                #where_clause
                {
                    #[allow(dead_code)]
                    #[inline]
                    #vis
                    fn #pinned_ident<'__> (
                        self: easy_pin::core::pin::Pin<&'__ Self>,
                    ) -> easy_pin::core::pin::Pin<&'__ #ty>
                    {
                        unsafe {
                            self.map_unchecked(|slf| &slf.#ident)
                        }
                    }
                }
            }

            // &mut _
            let pinned_ident_mut = Ident::new(
                &format!("pinned_{}_mut", ident),
                ident.span(),
            );
            render_spanned! { ident.span() =>
                impl #impl_generics
                    #struct_name #ty_generics
                #where_clause
                {
                    #[allow(dead_code)]
                    #[inline]
                    #vis
                    fn #pinned_ident_mut<'__> (
                        self: easy_pin::core::pin::Pin<&'__ mut Self>,
                    ) -> easy_pin::core::pin::Pin<&'__ mut #ty>
                    {
                        unsafe {
                            self.map_unchecked_mut(|slf| &mut slf.#ident)
                        }
                    }
                }
            }
        },

        // Pin projection
        | SpecialField {
            pin_transitiveness: Unpinned,
            vis,
            ty,
        } => {
            // & _
            let unpinned_ident = Ident::new(
                &format!("unpinned_{}", ident),
                ident.span(),
            );
            render_spanned! { ident.span() =>
                impl #impl_generics #struct_name #ty_generics #where_clause {
                    #[allow(dead_code)]
                    #[inline]
                    #vis
                    fn #unpinned_ident<'__> (
                        self: easy_pin::core::pin::Pin<&'__ Self>,
                    ) -> &'__ #ty
                    {
                        &self.get_ref().#ident
                    }
                }
            }

            // &mut _
            let unpinned_ident_mut = Ident::new(
                &format!("unpinned_{}_mut", ident),
                ident.span(),
            );
            render_spanned! { ident.span() =>
                impl #impl_generics
                    #struct_name #ty_generics
                #where_clause
                {
                    #[allow(dead_code)]
                    #[inline]
                    #vis
                    fn #unpinned_ident_mut<'__> (
                        self: easy_pin::core::pin::Pin<&'__ mut Self>,
                    ) -> &'__ mut #ty
                    {
                        unsafe {
                            &mut self.get_unchecked_mut().#ident
                        }
                    }
                }
            }
        },
    });

    ret
}
