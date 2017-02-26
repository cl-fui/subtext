# SUBTEXT 

SubText is a Lispy, mostly-text-based user interface. 

SubText 'entangles' CL code with arbitrary pieces of text, enabling simple, flexible, and familiar ad-hoc user interfaces (combined with an Emacs-inspired keyboard interface).  The entangled text/code entities (referred to as 'subtexts') can bind keys, modify text, or run arbitrary code.


## Demo and Screenshot

To experience the (very early) demo, clone the repo and (ql:quickload :subtext)(in-package :subtext)(demo)

![screenshot](Screenshot.png?raw=true) 

## Why?

Fluid, loosey-goosey editable text augmented with code makes for very flexible and efficient use of space and does not lock the developer into the same 'look-and-feel' box dictated by the OS vendor.  See [Background and Motivation](https://github.com/stacksmith/subtext/wiki/Background-and-Motivation)

## What's it like to work with it?

SubText is built on top of GTK.  GTK text buffers already feature MARKS (locations in text that are preserved across edits) and TAGS (ranges of text with certain attributes).  Now we add the idea of a subtext, a piece of tagged text bound to a CLOS object.

Subtexts can be output to a stream, placed inside other subtexts, bind keys, and respond to user input at an incredibly fine level.  Alternatively, the class hierarchy of presentations - as well as physical position of instances inside other instances on the screen - can provide useful defaults. 

Subtexts may also programmatically alter the look or the content of the text they control, providing feedback, displaying up-to-date information.

The GTK buffer looks like a stream, so printing mostly works like it always does.  A [utility function `prin`](https://github.com/stacksmith/subtext/wiki/PRIN) is provided for bulk output of tagged text:

```lisp
(prin out "Hello " (tg blue "Cruel ") "world")

(prin out "Press " (pr button (:code (lambda () (...))) "HERE") " now!")

```
Here, we output some tagged text, and create a button with a lambda for action.

## And visually?

Since SubText is GTK, anything you can do with GTK you can do with SubText.  In fact, menus, buttons and other GTK widgets can be dropped right into the text buffer!

## Status

This is an extremely early prototype, here mainly as proof-of-concept.  Many changes are taking place; I do not recommend taking anything here for granted.

