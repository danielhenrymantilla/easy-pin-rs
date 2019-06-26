#![allow(unused)]

use ::std::{
    pin::Pin,
    future::Future,
    task::{Context, Poll},
};

use ::easy_pin::easy_pin;

#[easy_pin]
struct Map<Fut, F> {
    #[transitively_pinned]
    future: Fut,

    #[unpinned]
    f_opt: Option<F>,
}

impl<Fut, F, Ret> Future for Map<Fut, F>
where
    Fut : Future,
    F : FnOnce(Fut::Output) -> Ret,
{
    type Output = Ret;

    fn poll (
        mut self: Pin<&'_ mut Self>,
        cx: &'_ mut Context,
    ) -> Poll<Self::Output>
    {
        match self.as_mut().pinned_future_mut().poll(cx) { //
            | Poll::Pending => Poll::Pending,
            | Poll::Ready(output) => {
                let f =
                    self.unpinned_f_opt_mut()
                        .take()
                        .expect(concat!(
                            "Map must not be polled after ",
                            "it returned `Poll::Ready`",
                        ));
                Poll::Ready(f(output))
            },
        }
    }
}

fn main ()
{}
