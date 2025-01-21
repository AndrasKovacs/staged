
{-
Building:
- I used Agda 2.8.0 (github master, but I don't actually rely on anything fancy)
  and standard library version 2.2

Formalization of the source language.
-}

open import Lib
open import Syntax            -- typed syntax of the language
open import Interpreter       -- fuelled interpreter (as operational semantics)
open import Renaming          -- definition & properties of variable renaming
open import Bisimilarity      -- notion of strong bisimilarity of terms
open import Bisimilarities    -- an assortment of proven bisimilarities
