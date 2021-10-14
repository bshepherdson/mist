
## Next steps

Rationals, GC and UI. Probably in that order.

### Rational numbers

Pretty straightforward, they just make a bunch of graphical things easier.

### GC



### UI

I need a scheme for drawing to one of:

- The DOM
- A Canvas(Context).
- A `<pre>`, in terminal fashion.

The last is the least useful on the web, but easiest and the most
Mocha86k-friendly if I'm sticking to the original intent here.

But if I want to use this for the Shadow Hunters game, which seems legit, it'll
need to be implemented. It's fast enough to get the job done on a Canvas I
think. I'm not animating anything, so it's all good.

#### Canvas design

The Smalltalk style is `Morph >> drawOn: aCanvas`, where `Canvas` has things
like `fillRect: aRectangle color: aColor`. Those would be primitives that hand
off to the `CanvasRenderingContext2D`.

That seems legit, and also reusable, since it decouples the Smalltalk-level UI
from how things actually get rendered.

#### Hi DPI

This is a pain point for font rendering in Canvas generally, but it can be
sidestepped by detecting the display resolution and monkey-patching the Canvas
in the DOM to have a bigger native size and scaling its apparent size back down.

Since the pixel-pushing is done at the native level on the Canvas context,
there's no performance implications for the Smalltalk level. There's no
buffering, though, so some care is required.

