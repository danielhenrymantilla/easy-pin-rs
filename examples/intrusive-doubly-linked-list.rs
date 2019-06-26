#![feature(dropck_eyepatch)]

use ::easy_pin::{
    easy_pin,
    PinSensitive,
};

use ::pin_utils::pin_mut;

use ::std::{*,
    cell::Cell,
    pin::Pin,
};

mod lib {
    use super::*;

    type Link<'node, T> = PinSensitive<
        Cell<
            Option<
                Pin<&'node Node<'node, T>>
            >
        >
    >;

    #[easy_pin(
        Drop = "unsafe_no_impl",
    )]
    pub
    struct Node<'node, T : 'node> {
        #[unpinned]
        pub
        value: T,

        #[transitively_pinned]
        prev: Link<'node, T>,

        #[transitively_pinned]
        next: Link<'node, T>,
    }

    /// `Drop` that would have been generated except for the *dangling* 'node
    ///
    /// # Safety:
    ///
    ///   - right before 'node dangles, the destructor of the pointee clears
    ///     our pointer, setting it to None.
    unsafe
    impl<#[may_dangle] 'node, T : 'node> Drop
        for Node<'node, T>
    {
        fn drop (self: &mut Self)
        {
            unsafe {
                ::easy_pin::PinDrop::drop_pinned(
                    Pin::new_unchecked(self)
                )
            }
        }
    }

    impl<T> easy_pin::PinDrop for Node<'_, T>
    {
        #[inline]
        unsafe
        fn drop_pinned (
            self: Pin<&mut Self>,
        )
        {
            /// Implementation does not require `unsafe`
            fn drop_pinned<T> (this: Pin<&mut Node<'_, T>>) {
                eprintln!("Dropping {:#x?}", &*this as *const _);
                if let Some(p) = this.prev.get() { p.next.set(None); }
                if let Some(n) = this.next.get() { n.prev.set(None); }
            }

            drop_pinned(self)
        }
    }

    impl<T : fmt::Debug> fmt::Debug for Node<'_, T>
    {
        fn fmt (
            self: &'_ Self,
            stream: &'_ mut fmt::Formatter<'_>,
        ) -> fmt::Result
        {
            stream
                .debug_struct("Node")
                .field("value", &self.value)
                .field("prev", &self.prev.get().map(|p| p.get_ref() as *const _))
                .field("next", &self.next.get().map(|p| p.get_ref() as *const _))
                .finish()
        }
    }

    impl<'node, T : 'node> Node<'node, T>
    {
        #[inline]
        pub
        fn new (value: T) -> Self
        {
            Self {
                value,
                prev: PinSensitive::new(Cell::new(None)),
                next: PinSensitive::new(Cell::new(None)),
            }
        }

        pub
        fn set_next (
            self: Pin<&'node Self>,
            next: Pin<&'node Self>,
        )
        {
            if let Some(n) = self.next.get() { n.prev.set(None); }
            self.next.set(Some(next));
            if let Some(p) = next.prev.get() { p.next.set(None); }
            next.prev.set(Some(self));
        }

        #[inline]
        pub
        fn set_prev (
            self: Pin<&'node Self>,
            prev: Pin<&'node Self>,
        )
        {
            prev.set_next(self);
        }

        #[inline]
        pub
        fn remove (
            self: &'_ Self,
        )
        {
            if let Some(p) = self.prev.get() { p.next.set(None); }
            if let Some(n) = self.next.get() { n.prev.set(None); }
            self.prev.set(None);
            self.next.set(None);
        }

        #[inline]
        pub
        fn next (
            self: &'node Self,
        ) -> Option<&'node Self>
        {
            self.next.get().map(Pin::get_ref)
        }

        #[inline]
        pub
        fn prev (
            self: &'node Self,
        ) -> Option<&'node Self>
        {
            self.prev.get().map(Pin::get_ref)
        }

        pub
        fn iter_forwards (
            self: &'node Self,
        ) -> impl Iterator<Item = &'node Node<'node, T>>
        {
            iter::successors(
                Some(self),
                |current| current.next(),
            )
        }

        pub
        fn iter_backwards (
            self: &'node Self,
        ) -> impl Iterator<Item = &'node Node<'node, T>>
        {
            iter::successors(
                Some(self),
                |current| current.prev(),
            )
        }
    }
}
use self::lib::*;

fn main ()
{
    let x = Node::new(42); // |x|
    let y = Node::new(27); // |y|
    let z = Node::new(0);  // |z|
    pin_mut!(x, y, z);
    let x = x.as_ref();
    let y = y.as_ref();
    let z = z.as_ref();
    x.set_next(y); // |x -> y|
    z.set_prev(y); // |z <- y <- x|
    // |x -> y -> z|
    dbg!(x  .iter_forwards()
            .map(|n| (n as *const _, n))
            .collect::<Vec<_>>()
    );
    // |z <- y <- x|
    dbg!(z  .iter_backwards()
            .map(|n| (n as *const _, n))
            .collect::<Vec<_>>()
    );

    y.remove(); // |x|, |y|, |z|
    z.set_next(x); // |z -> x|

    dbg!(z  .iter_forwards()
            .map(|n| (n as *const _, n))
            .collect::<Vec<_>>()
    );

    x.set_next(z); // |x -> z -> x ...

    dbg!(z  .iter_forwards()
            .map(|n| (n as *const _, n))
            .take(5)
            .collect::<Vec<_>>()
    );
}
