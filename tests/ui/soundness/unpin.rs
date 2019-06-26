#![allow(unused)]

use ::easy_pin::{
    easy_pin,
    PinSensitive,
};
use ::core::{
    ptr::NonNull,
    marker::Unpin,
};

#[easy_pin]
pub
struct SelfReferential {
    #[transitively_pinned]
    string: PinSensitive<String>,

    #[unpinned]
    at_string: NonNull<String>,
}

impl Unpin for SelfReferential {}

fn main ()
{}
