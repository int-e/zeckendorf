## Zeckendorf Arithmetic and Finite Automata

An exploration.

### Requirements

- haskell-src-exts-qq from https://github.com/int-e/haskell-src-exts-qq
  (pulls in haskell-src-meta and haskell-src-exts)
- QuickCheck
- containers

### Files

- `Automaton.hs` -- implements finite automata
- `CodeGen.hs` -- generate Haskell code from automata
- `TH.hs` -- actual automata specification for simplification functions
- `Zeckendorf.hs` -- Zeckendorf arithmetic
- `NegaZeckendorf.hs` -- Nega-Zeckendorf arithmetic
- `Util.hs` -- useful functions
- `simps.hs` -- collects generated code for reference

### Overview

We write Zeckendorf numbers with the least significant bit to the left.

    z:101001 = 1*1 + 0*2 + 1*3 + 0*5 + 0*8 + 1*13 = 17

The key observation---which is by no means obvious---is that when adding
or subtracting (Nega-)Zeckendorf numbers, carries are limited to -1 and 1.
For example,

      z:    10001
    + z:    00101
      z:    10102   (result before carries)
            -+-+-+  (carries, + = 1, - = -1)
        ( +)-+-+-   (carries negated and shifted left)
        (+-)+-+-    (carries negated and shifted left twice)
    = z:(10)010101  (result after adding carries.)
    = z:    010101  (the extra 1 corresponds to F_0 = 0)

In `TH.hs`, `makeAutomaton` introduces an initial state `(0,[],0)`
and states `(i,cs,o)` where `i` is an input value (0..2), `o` is an
output value (0..1) and `cs` is a three-element list of the relevant
carries. A transition from one state to another is allowed if the
carries fit together. Transitions are labeled by pairs `(i,o)` of
possible input / output value pairs.

That automaton is then simplified and made deterministic. Since the output
values are unknown, we project the resulting automaton to its input values.
The result is made deterministic again. For each state of this third
automaton, a transition function is generated that also tracks the possible
output sequences.

### Todo

- expand explanation
- more documentation?
