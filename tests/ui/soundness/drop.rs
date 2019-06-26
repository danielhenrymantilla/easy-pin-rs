#![allow(unused)]

use ::easy_pin::{
    easy_pin,
    PinSensitive,
};
use ::core::ptr::NonNull;

#[easy_pin]
pub
struct SelfReferential {
    #[transitively_pinned]
    string: PinSensitive<String>,

    #[unpinned]
    at_string: NonNull<String>,
}

impl Drop for SelfReferential {
    fn drop (self: &'_ mut Self) {
        ::core::mem::replace(
            &mut self.string,
            PinSensitive::new("".into()),
        );
    }
}

fn main ()
{}
