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
    #[unpinned]
    string: PinSensitive<String>,

    #[unpinned]
    at_string: NonNull<String>,
}

fn main ()
{}
