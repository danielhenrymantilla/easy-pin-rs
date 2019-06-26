#![allow(unused)]

use ::easy_pin::{
    easy_pin,
    PinSensitive,
};
use ::core::ptr::NonNull;

#[easy_pin(Drop)]
pub
struct SelfReferential {
    #[transitively_pinned]
    string: PinSensitive<String>,

    #[unpinned]
    at_string: NonNull<String>,
}

impl Drop for SelfReferential {
    fn drop (self: &mut Self) {
        ::core::mem::replace(
            &mut self.string,
            PinSensitive::new("".into()),
        );
    }
}

fn main ()
{}
