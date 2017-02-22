# SUBTEXT 

SubText is an attempt to create a Lispy, mostly-text-based user interface.

'Entangling' arbitrary pieces of text with CL code makes text interactive.  Layered above GTK, SubText enables the creation of simple, flexible, and familiar ad-hoc user interfaces (especially when combined with an Emacs-like key processing interface).

## Demo and Screenshot

To experience the (very early) demo, clone the repo and (ql:quickload :subtext)(in-package :subtext)(demo)

![screenshot](Screenshot.png?raw=true) 

## Why?

Fluid, loosey-goosey editable text augmented with code makes for very flexible and efficient use of space and does not lock the developer into the same 'look-and-feel' box dictated by the OS vendor.  See [Background and Motivation](https://github.com/stacksmith/subtext/wiki/Background-and-Motivation)

## How does it work?

SubText is built on top of GTK.  GTK text buffers already feature MARKS (locations in text that are preserved across edits) and TAGS (ranges of text with certain attributes).  SubText adds the concept of a presentation (not to be confused with CLIM presentations), a piece of tagged text bound to a Common Lisp object.  Interacting with a presentation invokes CLOS methods on the bound Lisp object.  Conversely, the Lisp object may change the look or content of its text as seen by the user.

The buffer looks like a stream, so printing mostly works like it always does.  A [utility function `prin`](https://github.com/stacksmith/subtext/wiki/PRIN) is provided for bulk output of tagged text:

```lisp
(prin out "Hello " (tg blue "Cruel ") "world")

(prin out "Press " (pr button (:code (lambda () (...))) "HERE") " now!")

```

## And visually?

Since SubText is GTK, anything you can do with GTK you can do with SubText.  In fact, menus, buttons and other GTK widgets can be dropped right into the text buffer!

## Status

This is an extremely early prototype, here mainly as proof-of-concept.  Many changes are taking place; I do not recommend taking anything here for granted.

