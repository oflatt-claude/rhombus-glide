# glide-pptx

Direct manipulation of pict slideshows through PowerPoint or Keynote, instead of
through a GUI of our own.

`glide` gives you a window to drag slide elements around in. `glide-pptx` gives
you PowerPoint or Keynote for the same job: it translates a `.pptx` into a
[Pict](https://docs.racket-lang.org/pict/) program (Racket or Rhombus), exports
any pict program back to `.pptx`, and keeps the two in step while you edit
either one. Both directions are checked by rendering to PDF and diffing the
pages.

The point is the same as glide's: use a direct-manipulation editor for what it is
good at — dragging things until they look right — and code for what it is good at
— animation, abstraction, reuse, version control. The two share the hard part,
which is turning a drag into an edit of the source that leaves the rest of the
file alone.

```console
$ raco glide-pptx translate -o out talk.pptx
out/talk.rkt  (14 slides, 212 elements)

$ racket out/talk.rkt
wrote talk.pdf

$ raco glide-pptx export -o talk-edit.pptx out/talk.rkt
talk-edit.pptx  (14 slides)

$ raco glide-pptx verify talk.pptx
talk
  page    mean err    pixels off
  1       0.50%       0.87%        ok
  2       0.74%       0.98%        ok
  ...
  PASS
```

Export works on **any** pict program, not only generated ones. It is the mirror
of `picts->pdf`:

```racket
(require pict glide-pptx/export)

(picts->pptx (list slide-1 slide-2) "out.pptx" #:width 960 #:height 540)
```

## What the generated code looks like

Positions, sizes, colors and text come from the file; everything else is
ordinary Racket, so a slide is a `pict` you can compose with anything.

```racket
#lang racket/base
(require pict glide-pptx/runtime)

(define slide-width 959.976)
(define slide-height 540.0)

;; The theme font. Runs that name no typeface use this one, so restyling
;; the whole deck is one edit.
(current-default-font "Calibri")

(define slide-3
  (slide-canvas
   #:width slide-width #:height slide-height
   #:background (hex "FFFFFF")

   ;; TextBox 1 (id 2)
   (at 50.5 28.75
       (textbox #:width 792.0 #:height 72.0 #:wrap? #f #:autofit 'grow
                (para* (run* "Pipeline" #:size 40.0 #:bold? #t #:color (hex "1F3B63")))))

   ;; Rounded Rectangle 2 (id 3)
   (at 57.5 158.5
       (shape-pict #:width 172.8 #:height 86.5 #:shape "roundRect"
                   #:fill (hex "4472C4")
                   #:body (body* #:anchor 'center
                                 (para* #:align 'center
                                        (run* "pptx" #:size 22.0 #:bold? #t
                                              #:color (hex "FFFFFF"))))))))
```

The same deck in Rhombus (`--rhombus`), against the same runtime:

```rhombus
#lang rhombus/and_meta
import:
  lib("glide-pptx/runtime.rhm") open

def slide_3 = slide_canvas(
  ~width: slide_width, ~height: slide_height,
  ~background: hex("FFFFFF"),
  // Rounded Rectangle 2 (id 3)
  at(57.5, 158.5,
     shape_pict(~width: 172.8, ~height: 86.5, ~shape: "roundRect", ~fill: hex("4472C4"),
                ~body: body(~anchor: #'center,
                            para(~align: #'center,
                                 run("pptx", ~size: 22.0, ~bold: #true,
                                     ~color: hex("FFFFFF")))))))
```

Each element keeps the shape id and name PowerPoint gave it, as a comment. That
is what a future write-back will match on, and in the meantime it is how you
find the thing you just clicked on in the editor.

## Commands

| Command | What it does |
| --- | --- |
| `translate deck.pptx [-o dir] [--rhombus]` | emit a program plus the images it uses |
| `export program.rkt [-o out.pptx]` | write a `.pptx` from the program's slide picts |
| `sync program.rkt deck.pptx [-n]` | merge the deck's edits back into the program |
| `watch program.rkt [--app keynote]` | keep both in step, in both directions |
| `render deck.pptx [-o dir]` | render straight to PDF, skipping code generation |
| `verify deck.pptx ...` | render both ways and report per-page differences |
| `ir deck.pptx` | print the intermediate representation |
| `presets` | list the shape geometries drawn exactly |

Anything drawn approximately is reported on stderr rather than silently
approximated:

```console
$ raco glide-pptx translate deck.pptx
2 notes about approximate rendering:
  - graphicFrame Chart 4: unsupported content (chart or diagram), drawn as an empty box
  - picture Image 9: unresolved image rId7
```

## How export works

A pict is drawn through `record-dc%`, whose `get-recorded-datum` returns the
drawing as an interpretable command stream. That stream keeps **high-level
operations rather than flattened paths** — a rectangle records as
`draw-rectangle`, text as `draw-text` with its font — so each one becomes a real
PowerPoint object:

| recorded op | becomes |
| --- | --- |
| `draw-rectangle`, `draw-rounded-rectangle` | `prstGeom` rect / roundRect |
| `draw-ellipse` | `prstGeom` ellipse |
| `draw-path`, `draw-polygon`, `draw-lines`, `draw-arc` | `custGeom` with Beziers |
| `draw-text` | a text box you can retype, in the same typeface |
| `draw-bitmap` | a picture, with a PNG media part |
| brushes and pens | `solidFill`, `gradFill`, `a:ln` with dash and cap |
| transforms | folded into a matrix and baked into coordinates |

Exporting a slide of two shapes and two strings produces five `p:sp` shapes with
four `prstGeom` rects, one `prstGeom` ellipse and two editable text runs — no
`custGeom`, no rasterization.

Going through SVG would not work: `svg-dc%` renders that same slide as 24
`<path>` elements with **zero** `<rect>` and **zero** `<text>`, because cairo
outlines every glyph. See `docs/export-design.md`.

What the flattened path cannot do: text does not reflow (one box per `draw-text`
call, `wrap="none"`), DrawingML has no arbitrary clipping, and sheared text has
to rasterize.

### When the program's structure is known

A pict built with this runtime carries a description of how it was built, so it
does not have to be flattened at all. `roundRect` stays a `roundRect` rather than
becoming a path, a paragraph stays a paragraph, and every element carries the
shape name it had in PowerPoint:

```racket
(at 57.6 158.4 #:tag "Rounded Rectangle 2"
    (shape-pict #:width 172.8 #:height 86.4 #:shape "roundRect" ...))
```

becomes `<p:sp>` with `<p:cNvPr name="Rounded Rectangle 2"
descr="glide-pptx:Rounded Rectangle 2"/>`, a `prstGeom prst="roundRect"`, and a
reflowable `<a:txBody>`.

The effect is measurable. A deck exported from a generated program reproduces the
**original** `.pptx` better than our own renderer does, because PowerPoint is
handed back its own presets and text bodies rather than our approximation of
them:

| deck | our render vs original | exported deck vs original |
| --- | --- | --- |
| 01-placeholders | 0.20 – 0.74% | 0.00 – 0.16% |
| 02-text | 0.67% | 0.15% |
| 03-shapes | 0.41 – 1.48% | 0.40 – 0.45% |
| 04-pictures-groups | 0.30 – 0.35% | 0.00 – 0.11% |
| 05-realistic | 0.70 – 1.23% | 0.14 – 0.22% |

Two of those pages come out pixel-identical. The carrier is a `pict` subtype, so
it survives every pict combinator except `launder`; anything without a
descriptor still exports through the flattened path.

## Keeping a program and a deck in step

`watch` is the parent process for the whole loop:

```console
$ raco glide-pptx watch talk.rkt --app keynote
watching
  program talk.rkt
  deck    talk.key
  app     keynote
program changed -> regenerating talk.pptx
  14 slides written
deck changed -> merging into talk.rkt
  slide 3  moved        "Rounded Rectangle 2"  -> 100.0,200.0 172.8x86.4
  1 applied, 0 reported
```

Saving the program regenerates the deck and tells the app to show it. Saving the
deck merges its geometry back into the program's source.

Change detection is by content hash rather than filesystem event. That costs a
poll and buys three things: it works for a Keynote `.key`, which is a directory;
it collapses the burst of writes an editor makes when saving; and it is the loop
guard, since a file we wrote ourselves hashes to what we expect and so does not
look like someone else's edit.

### What a merge is allowed to do

A three-way merge against a base -- the state both sides agreed on at the last
sync, kept in `talk.sync.rktd` next to the program and belonging in version
control. The rule that keeps the loop stable falls out of the intent:
**PowerPoint owns geometry, code owns everything else.** With the two sides
owning disjoint properties, the ordinary cycle has no conflicts at all.

Every edit is one of two things: replace a literal, or wrap an expression in a
known form. Nothing restructures the program.

| what changed | position is a literal | position is computed |
| --- | --- | --- |
| drag | patch the two numbers on `at` | insert or update a `#:nudge` correction |
| resize | patch `#:width` / `#:height` | patched if the size is a literal |
| text | patch the string literal | reported |

When a position is computed there is no number to rewrite, so the drag is
recorded as a correction on `at` instead:

```racket
(at left 60.0 #:tag "Box" #:nudge (list 160.0 40.0)
    (shape-pict #:width 100.0 #:height 40.0 ...))
```

The program's own layout logic is untouched, and the correction says plainly
that a hand adjustment was made. Being one argument rather than a wrapper is
deliberate: **a second drag updates those two numbers**, so corrections cannot
stack up the way nested pads do. Dragging the same element twice leaves exactly
one `#:nudge`, which the tests check.

A drag on `(at 57.6 158.4 #:tag "Rounded Rectangle 2" ...)` changes exactly that
one line and no other; `(at margin (+ top 20) ...)` is reported and left alone,
because computed layout is a decision the code is making on purpose.

### Editors, and how much they can be trusted

| app | native format | how the deck is read back | tested |
| --- | --- | --- | --- |
| PowerPoint | `.pptx` | it *is* the deck — nothing to do | no (needs macOS) |
| Keynote | `.key` | AppleScript `export ... as Microsoft PowerPoint` | no (needs macOS) |
| LibreOffice | `.pptx` | it is the deck | yes |
| `none` | — | you drive your editor yourself | yes |

PowerPoint is the easiest target, since its own format is the interchange
format. Keynote never saves `.pptx`, so the loop exports one out of it, and it
has no reload API — the document is closed and reopened, which loses the current
slide and selection.

Identity is layered: `descr` (alt text), then the shape `name`, then signature
matching, which needs no annotation at all. Measured through a LibreOffice
`pptx` round trip, `name` and `descr` on ordinary shapes **do** survive — the
decks we export contain no placeholders, and it is placeholder shapes that get
renamed to "PlaceHolder 1" and stripped. Shape ids are renumbered either way, so
they are never a key.

## How verification works

`verify` converts the deck to PDF with headless LibreOffice, renders our own
PDF, rasterizes both with the same rasterizer at the same resolution, and
compares pages pixel by pixel. It reports two numbers per page:

- **mean err** — average absolute channel difference over the page.
- **pixels off** — share of pixels differing by more than 25%, which is the
  number that tracks "something moved" rather than "text is antialiased
  differently".

It also writes, per page, a diff image (grey where the two agree, red where they
do not) and a montage stacking reference, ours, and the diff.

### It converges rather than drifts

Round-tripping repeatedly does not accumulate error. Measured over five
generations of deck -> program -> deck:

| | |
| --- | --- |
| exported slide XML | **byte-identical from generation 1 onward** |
| generated program | a fixed point from generation 2, modulo the input filename |
| pixel error against the original | **0.182% at every generation** |

The residual is a one-time conversion difference, and it is not arithmetic: EMU
is a 12700th of a point and generated source rounds to a thousandth, about
0.0001pt, or 0.00013px at 96 dpi. It is **text laid out twice by two engines** --
our renderer measures with cairo to decide a box, and then PowerPoint re-lays the
paragraphs inside it. On a real talk with 203 text bodies, small disagreements
about line breaking, ascent and autofit scale account for nearly all of it.

Exact equality is not the goal and is not reachable. LibreOffice is itself an
approximation of PowerPoint, and two text engines will never agree to the pixel.
The current fixtures land between 0.2% and 1.5% mean error; `tests/fidelity.rkt`
pins a per-deck budget just above that, so a layout regression trips it while
rasterizer noise does not.

Font substitution is what makes the comparison meaningful at all: the fixtures
ask for Calibri, and both sides get Carlito through fontconfig. Install
`fonts-crosextra-carlito` and `fonts-crosextra-caladea` alongside
`fonts-liberation`, or the reference will use a fallback with different metrics
and every number above will be noise.

## What is handled

- **Package** — OPC container, relationship resolution, media extraction.
- **Inheritance** — a placeholder's geometry and text properties resolved
  through slide → layout → master → theme, including the master's `txStyles`
  and the presentation's `defaultTextStyle`. Most real slides state almost
  nothing on the shape itself, so this is the difference between usable output
  and garbage.
- **Theme** — color scheme through the master's `clrMap`, the `phClr`
  indirection inside the format scheme, `tint`/`shade`/`lumMod`/`lumOff`/
  `satMod`/`alpha` transforms, major and minor fonts, and `fillRef`/`lnRef`
  style references.
- **Shapes** — 53 preset geometries drawn exactly, custom geometry paths,
  solid/gradient/pattern/image fills, outlines with dash and cap, rotation,
  flips, groups with their own child coordinate space, connectors, tables.
- **Text** — runs with family, size, weight, slant, underline, strike, color,
  letter spacing, caps and super/subscript; paragraph alignment, indents,
  hanging bullets (character and auto-numbered), line spacing, space before and
  after; word wrap with in-word breaking, vertical anchoring, and PowerPoint's
  cached autofit scale.
- **Slides** — background fills, and the layout's and master's own graphics
  drawn behind the slide, kept separate from the slide's own shapes.

## What is not handled yet

- Charts and SmartArt (`graphicFrame` content other than tables) draw as an
  empty box and are reported.
- Effects: shadows, glow, reflection, 3-D, soft edges.
- Animations and transitions — deliberately, since they are the part that
  belongs in code rather than in the file.
- Vertical and rotated-90 text layout (`bodyPr` `vert`), and right-to-left runs.
- Justified text falls back to left alignment.

## Layout

```
glide-pptx/
  units.rkt        EMU, hundredths of a point, 60000ths of a degree
  xml-util.rkt     namespace-tolerant xexpr queries
  opc.rkt          zip container and relationship resolution
  ir.rkt           the intermediate representation, in points
  theme.rkt        color scheme, color map, transforms, format scheme
  drawing.rkt      fills, outlines, transforms
  text.rkt         text bodies and property inheritance
  shapes.rkt       shape tree to IR
  parse.rkt        pptx to deck IR
  geometry.rkt     preset shape names to drawing paths
  runtime.rkt      IR to picts: text layout, paint, PDF output
  draw-ir.rkt      the display list: a model of drawing, not of PowerPoint
  record-adapt.rkt record-dc% datum to display list; the only module that
                   knows the datum's shape
  pptx-write.rkt   display list to DrawingML and an OPC package
  export.rkt       picts->pptx, the mirror of picts->pdf
  runtime.rhm      Rhombus naming over the same runtime
  render.rkt       IR to picts, via the runtime
  emit-common.rkt  what to emit, shared by both back ends
  emit-racket.rkt  Racket surface syntax
  emit-rhombus.rkt Rhombus surface syntax
  verify.rkt       LibreOffice reference, rasterizing, image diffing
  tagged.rkt       a pict subtype carrying how it was built
  semantic.rkt     that structure to display-list items that keep their meaning
  sync-state.rkt   the vocabulary a program and a .pptx are compared in
  sync.rkt         three-way merge and source patching at syntax locations
  watch.rkt        the parent process, and the per-app adapters
  main.rkt         command line
tools/
  make-test-decks.py   generates the fixtures with python-pptx
tests/
  unit.rkt        units, xml, relationships, theme colors, geometry
  roundtrip.rkt   emitted programs reproduce the direct render
  fidelity.rkt    per-deck budgets against LibreOffice
  export.rkt      exported .pptx matches the picts it came from
  corpus.rkt      the whole pipeline over several hundred real decks
  sync.rkt        a drag comes back as a literal, and computed layout does not
  watch.rkt       the loop, driven from both sides
```

The direct renderer and both emitters go through one runtime, so a fidelity fix
lands everywhere at once and `tests/roundtrip.rkt` can demand that a generated
program draw exactly what the renderer draws.

## Building from source

Racket and Rhombus, both from source:

```console
git clone --depth 1 https://github.com/racket/racket.git
cd racket && make base            # builds Chez Scheme, then Racket CS
export PATH=$PWD/racket/bin:$PATH
raco pkg install --auto pict-lib

git clone --depth 1 https://github.com/racket/rhombus.git
cd rhombus && raco pkg install --auto \
  ./enforest-lib ./shrubbery-lib ./rhombus-lib \
  ./rhombus-draw-lib ./rhombus-pict-lib ./rhombus-slideshow-lib
```

Then this package, and the tools verification needs:

```console
raco pkg install --auto --link /path/to/glide-pptx
sudo apt-get install libreoffice-impress poppler-utils \
  fonts-liberation fonts-crosextra-carlito fonts-crosextra-caladea
```

`racket/draw` needs system cairo, pango and fontconfig; on Debian and Ubuntu
that is `libcairo2 libpango1.0-dev libgdk-pixbuf-2.0-0`.

Regenerating the fixtures needs `python-pptx`:

```console
python3 -m venv .venv && .venv/bin/pip install python-pptx
.venv/bin/python tools/make-test-decks.py
```

## Tests

```console
raco test tests/all.rkt      # everything
raco test tests/unit.rkt     # fast, no external tools
```

`tests/fidelity.rkt` and `tests/export.rkt` need LibreOffice and poppler;
`tests/unit.rkt` needs nothing.

`tests/corpus.rkt` runs the whole pipeline -- import, render, draw, export,
re-import -- over LibreOffice's own pptx regression suite, several hundred decks
each aimed at one awkward corner of the format. It checks crash-freedom rather
than fidelity, since those files are deliberately strange, and it prints an
inventory of everything drawn approximately. The decks are not committed; fetch
them with

```console
$ tools/fetch-corpus.sh
399 decks present
```

and the test says so and passes when they are absent. It found two real bugs on
its first run: a custom path that opens with a line rather than a move, and a
picture whose relationship resolves to nothing.

## Where this is going

The loop works for geometry and text on literals. What is left:

1. **A `nudge` wrapper for computed layout.** A drag on `(vc-append 5 a b)` has no
   number to patch, but it is expressible as a bbox-preserving pad --
   `(inset p dx dy (- dx) (- dy))`, measured to move the drawing exactly while
   leaving the enclosing combinator alone. It converges: after one drag the
   element has literal numbers from then on.
2. **Shapes added or deleted in the editor.** Reported now; inserting a generated
   `(at ...)` at the right z-position is the next step, and a deletion should
   comment code out rather than remove it.
3. **Patching the original package** instead of synthesizing one, so a deck with
   charts or SmartArt keeps them across the loop even though we cannot render
   them.
4. **Testing the Keynote and PowerPoint adapters**, which needs a Mac.
5. **`--stages`**, one slide per animation step, for a presentable deck.
   `rhombus/pict`'s `snapshot(epoch)` hands that over directly.
