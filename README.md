# SUBTEXT 

SubText is a Lispy, mostly-text-based user interface. 

SubText 'entangles' CLOS objects with arbitrary pieces of text, enabling simple, flexible, and familiar ad-hoc user interfaces.  

These text/code entities, referred to as `contexts`, provide the setting for user interaction within their bounds.  They can bind keys, visually modify their text, or execute arbitrary code.  The CLOS hierarchies, together with the runtime placement within other contexts, provide useful defaults.

SubText provides the plumbing connecting the contexts to the GTK environment and controls their interaction, largely by following the insertion point of the buffer and notifying the contexts involved.

## Demo and Screenshot

To experience the (very early) demo, clone the repo and (ql:quickload :subtext)(in-package :subtext)(demo)

![screenshot](Screenshot.png?raw=true) 

## Why?

Fluid, loosey-goosey editable text augmented with code makes for very flexible and efficient use of space and does not lock the developer into the same 'look-and-feel' box dictated by the OS vendor.  See [Background and Motivation](https://github.com/stacksmith/subtext/wiki/Background-and-Motivation)

## What's it like to work with it?

SubText is built on top of GTK.  GTK text buffers already feature MARKS (locations in text that are preserved across edits) and TAGS (ranges of text with certain attributes).  Now we add the idea of a context, a piece of tagged text bound to a CLOS object.

Contexts are CLOS objects.  The class hierarchy, as well as their physical position within other contexts on the screen ultimately decide how they react to user 

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

