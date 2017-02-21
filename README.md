# SUBTEXT 

SubText is an attempt to create a Lispy, mostly-text-based user interface.

'Entangling' runs of text with CL code makes text interactive.  Combined with an Emacs-like command processing system, SubText enables the creation of simple, flexible, and familiar ad-hoc user interfaces.

## Demo and Screenshot

To experience the (very early) demo, clone the repo and (ql:quickload :subtext)(in-package :subtext)(demo)

![screenshot](Screenshot.png?raw=true) 

## Why?

Presentation-based interfaces may well be the future of computing.  Fluid, loosey-goosey editable text combined with 'live' text makes for very efficient use of space and does not lock the developer into a 'look-and-feel' dictated by the OS vendor.  See [Background and Motivation](https://github.com/stacksmith/subtext/wiki/Background-and-Motivation)


## How does it work?

SubText is built on top of GTK.  GTK text buffers feature MARKS (locations in text that are preserved across edits) and TAGS (ranges of text with certain attributes).  SubText adds the concept of a presentation (not to be confused with McCLIM presentations).  A presentation is a piece of tagged text bound to a Common Lisp object.  Interacting with a presentation invokes CLOS methods on the bound Lisp object; conversely, the Lisp object may change the look or content of its text as seen by the user.

A special function `prin` outputs text, inserting tags and presentations into the stream.

```lisp
(prin out "Hello " (tg blue "Cruel ") "world")

(prin out "Press " (pr button (:code (lambda () (...))) "HERE") " now!")

```

## And visually?

Since SubText is GTK, anything you can do with GTK you can do with SubText.

## Status

This is an extremely early prototype, here mainly as proof-of-concept.  Many changes are taking place; I do not recommend taking anything here for granted.

