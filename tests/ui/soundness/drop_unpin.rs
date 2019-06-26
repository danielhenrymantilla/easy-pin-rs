#![feature(trivial_bounds)]
#![allow(unused)]

use ::easy_pin::{
    easy_pin,
    PinDrop,
    PinSensitive,
};
use ::core::{
    pin::Pin,
    ptr::NonNull,
};

#[easy_pin(Drop)]
pub
struct SelfReferential {
    #[transitively_pinned]
    string: PinSensitive<String>,

    #[unpinned]
    at_string: NonNull<String>,
}

impl PinDrop for SelfReferential {
    unsafe
    fn drop_pinned (mut self: Pin<&mut Self>)
    {
        let at_string_mut: &mut PinSensitive<String> = if true {
            self.pinned_string_mut().get_mut()
        } else {
            &mut self.get_mut().string
        };
        ::core::mem::replace(
            at_string_mut,
            PinSensitive::new("".into()),
        );
    }
}

fn main ()
{}
