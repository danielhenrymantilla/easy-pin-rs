#![feature(trivial_bounds)]

#![allow(unused)]

use ::easy_pin::{
    easy_pin,
    PinSensitive,
};
use ::core::ptr::NonNull;

#[repr(packed)]
#[easy_pin]
pub
struct SelfReferential {
    #[transitively_pinned]
    string: PinSensitive<String>,
}

fn main ()
{}
