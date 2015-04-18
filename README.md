# ThreeDee
## Three-dimensional category theory diagram server using Haskell and Yesod.

This program displays a single 3-d category theory diagram, one that illustrates vertical composition of natural transformations. 
It lets you rotate the diagram in 3-d using the mouse.
For more information see my blog post about [Natural Transformations](http://bartoszmilewski.com/2015/04/07/natural-transformations/)
in category theory.

You can compile and run this program online in the [FP Complete IDE](https://www.fpcomplete.com/user/bartosz/threedee).

Or, to run it locally, install the [Haskell Platform](https://www.haskell.org/platform/) 
and the [Yesod web library](http://www.yesodweb.com/page/quickstart).

Note: I don't like programming in JavaScript, so I implemented the bare minimum of the client side. 
I did the handling of 3-d rotations on the server in response to the client dragging the mouse. 
Of course, you can't expect good performance if you're running it remotely.
It's a miracle that it works at all. But if you run the web server locally, and rotate the diagram in your browser, 
the performance if fine.
