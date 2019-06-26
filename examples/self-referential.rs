use ::easy_pin::{
    easy_pin,
    PinSensitive,
};

use ::std::{*,
    pin::Pin,
    ptr::NonNull,
};

mod lib {
    use super::*;

    #[easy_pin(
        // Cannot derive Unpin when a #[transitively_pinned] is
        // unconditionnally `!Unpin`.
        //
        // # Safety:
        //
        //   - There is no `Unpin` implementation within the crate.
        Unpin = "unsafe_no_impl",
    )]
    pub
    struct SelfReferential {
        #[transitively_pinned]
        string: PinSensitive<String>,

        #[unpinned]
        at_string: NonNull<String>,
    }

    impl SelfReferential {
        pub
        fn new (string: impl Into<String>) -> Pin<Box<Self>>
        {
            let mut pinned_box = Box::pin(Self {
                string: PinSensitive::new(string.into()),
                at_string: NonNull::dangling(),
            });
            let at_string: NonNull<String> =
                pinned_box.as_ref()
                    .pinned_string()
                    .pinned_address()
            ;
            *pinned_box.as_mut().unpinned_at_string_mut() = at_string;
            pinned_box
        }

        #[inline]
        pub
        fn at_string<'_1> (self: Pin<&'_1 Self>) -> &'_1 String
        {
            unsafe {
                // Safety: the only way to get a Pin<&Self> is through
                // Self::new().as_ref(), which ensures the pointer is well-formed.
                self.get_ref().at_string.as_ref()
            }
        }
    }
}
use self::lib::*;

fn main ()
{
    let self_referential = SelfReferential::new("Hello, world!");
    dbg!(self_referential.as_ref().at_string());
}
