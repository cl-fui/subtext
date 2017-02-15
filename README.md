# SUBTEXT 

SubText is an attempt to create a Lispy, mostly-text-based user interface.

'Entangling' runs of text with CL code makes text interactive.  Combined with an Emacs-like command processing system, SubText enables the creation of simple, flexible, and familiar ad-hoc user interfaces.

## Why?

Presentation-based interfaces may well be the future of computing.  Fluid, loosey-goosey editable text combined with 'live' text makes for very efficient use of space and does not lock the developer into a 'look-and-feel' dictated by the OS vendor. 


![alt text](https://github.com/stacksmith/subtext/blob/master/Screenshot.png")

## How does it work?

SubText is built on top of GTK.  GTK text buffers feature MARKS (locations in text that are preserved across edits) and TAGS (ranges of text with certain attributes).  SubText adds the concept of a presentation (not to be confused with McCLIM presentations).  A presentation is a piece of tagged text bound to a Common Lisp object.  Interacting with a presentation invokes CLOS methods on the bound Lisp object; conversely, the Lisp object may change the look or content of its text as seen by the user.

Work is being done on representing tags and presentations easily in Lisp.  Right now, output functions are a bit verbose, but there is much room for improving the syntax as we are dealing with Lisp.

```lisp
(with-tag ("bg-greenish" stream) (princ "SubText" stream))

(with-pres (textbutton (:code (lambda () (exec "SHOW DEVICES DAD40"))))
  (princ "NEWS DEMO" stream))

```
## And visually?

Since SubText is GTK, anything you can do with GTK you can do with SubText.

## Status

This is an extremely early prototype, here mainly as proof-of-concept.  Many changes are taking place; I do not recommend taking anything here for granted.

