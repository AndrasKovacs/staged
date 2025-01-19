
# Yoneda Logical Framework

## Overview

It's common in the metatheory of type theories to define models of object
theories, do some constructions while working internally in a model, then
externalize said constructions. For internal constructions, we can always use
the "raw" components of a model, including De Bruijn indices, but that's very
tedious. Instead, we want to use "usual" syntax with named binders.

I describe here a type theory where for each internally defined model we get an
internal notation and a way to switch between internal and external views.
Similar internal notations have been used many times, but to my knowledge,
without a rigorous specification.

I call the theory **Yoneda Logical Framework** or YLF, because the
internal/external switch is given simply by the Yoneda embedding.

## Syntax of YLF

YLF has multiple modes, not unlike modal type theories, although it does not
quite fit into any existing framework for modalities.

- The "external" model is a vanilla extensional TT.
- For each model M of a second-order algebraic theory that's defined in the external mode,
  there is an "internal" mode. In this mode, we have *dual context*,





<!-- If we ignore internal notations, YLF is just a vanilla extensional type -->
<!-- theory. For simplicity, I assume a universe `Set` with `Set : Set`. In the -->
<!-- following, I use "external" to refer to this mode -->
