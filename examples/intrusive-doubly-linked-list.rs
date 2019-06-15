use ::easy_pin::{
    easy_pin,
};

use ::pin_utils::pin_mut;

use ::std::{*,
    cell::Cell,
    pin::Pin,
};

mod lib {
    use super::*;

    #[easy_pin(Unpin, Drop)]
    pub
    struct Node<T> {
        #[unpinned]
        pub
        value: T,

        #[unpinned]
        prev: Cell<*const Node<T>>,

        #[unpinned]
        next: Cell<*const Node<T>>,
    }

    impl<T> easy_pin::PinDrop for Node<T> {
        #[inline]
        unsafe
        fn drop_pinned (
            self: Pin<&mut Self>,
        )
        {
            self.prev.get().as_ref().map(|p| p.next.set(ptr::null()));
            self.next.get().as_ref().map(|n| n.prev.set(ptr::null()));
        }
    }

    impl<T> Node<T> {
        #[inline]
        pub
        fn new (value: T) -> Self
        {
            Self {
                value,
                prev: Cell::new(ptr::null()),
                next: Cell::new(ptr::null()),
            }
        }

        pub
        fn append<'prev, 'next> (
            self: Pin<&'prev Self>,
            next: Pin<&'next Self>,
        )
        {
            assert!(self.next.get().is_null());
            self.next.set(&*next);
            assert!(next.prev.get().is_null());
            next.prev.set(&*self);
        }

        #[inline]
        pub
        fn prepend<'next, 'prev> (
            self: Pin<&'next Self>,
            prev: Pin<&'prev Self>,
        )
        {
            prev.append(self);
        }

        #[inline]
        pub
        fn next<'__> (
            self: &'__ Self,
        ) -> Option<&'__ Self>
        {
            unsafe {
                self.next.get().as_ref()
            }
        }

        #[inline]
        pub
        fn prev<'__> (
            self: &'__ Self,
        ) -> Option<&'__ Self>
        {
            unsafe {
                self.prev.get().as_ref()
            }
        }
    }
}
use self::lib::*;

fn main ()
{
    let x = Node::new(42);
    let y = Node::new(27);
    pin_mut!(x, y);
    {
        let z = Node::new(0);
        pin_mut!(z);
        // x -> z
        x.as_ref().append(z.as_ref());
        // y <- z
        y.as_ref().prepend(z.as_ref());
        let mut cursor = x.as_ref().get_ref();
        loop {
            println!("cursor -> {}", cursor.value);
            if let Some(next) = cursor.next() {
                cursor = next;
            } else {
                break;
            }
        }
        println!("And back...");
        loop {
            println!("cursor -> {}", cursor.value);
            if let Some(prev) = cursor.prev() {
                cursor = prev;
            } else {
                break;
            }
        }
    }
    println!("Dropped 0.");
    let mut cursor = x.as_ref().get_ref();
    loop {
        println!("cursor -> {}", cursor.value);
        if let Some(next) = cursor.next() {
            cursor = next;
        } else {
            break;
        }
    }
}
