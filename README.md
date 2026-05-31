# Advent of Code 2025

My solutions to the 2025 [Advent of Code](https://adventofcode.com/2025), written mostly in OCaml.

## Running

Days 1 to 4 read their input from standard input (they only contain Part 2, Part 1 was not kept):

```sh
ocaml main.ml < input.txt
```

From day 5 onward, each day reads a local `input.txt` and is built with:

```sh
make
```

## Days 8 and 9

Both were written in [Uiua](https://www.uiua.org/), as an experiment with array programming.

Day 8 (runs in about 5 seconds):

```
&fras"input.txt"
Flatten ← □⊂⊃(♭°□⊣|°□⊢)
Dist    ← ⊂₃⊙⊙(√/+≡ⁿ2-)⊡1⊙(⊡1)⊃(⊙∘|(°□⊡0)⊙(°□⊡0))
Union   ← ⍣(⊂⊙Flatten)∘▽¬˜⊃(⊙∘|▽)⊙(=1≡°□+)⟜⊃(≡⍜(°□|∊)|≡⍜(°□|∊)⊙⋅∘)⊙°⊟⊙≡°□˜⊙∘
˙⊞Dist≡˜⊸⊟⇡⊸⧻≡□≡(⊜⋕⊸≠@,°□)⊜□⊸≠@\n
⍥₁₀₀₀(↘1⟜(Union⊡0))⊙≡□≡(↘1)⍆▽⊸≡(°□>0⊡0)/⊂≡≡(˜⊂↘¯1⟜(×<°⊟₃))
/×↙3⇌⍆≡(⧻°□)◌
```

Day 9, Part 1:

```
&fras"input.txt"
Area ← ×∩(+1⌵-)⊃(⋅⊙⋅∘|⊙⋅⊙◌)°⊟⊙°⊟
/↥♭˙⊞Area≡(⊜⋕⊸≠@,°□)⊜□⊸≠@\n
```

For Part 2 I could not implement segment-square collision detection in Uiua (and it would not have been fast enough anyway), so I derived the answer from the Uiua intermediate results in a spreadsheet.

## Day 10

Solved in Python. In hindsight I should have used Z3.
