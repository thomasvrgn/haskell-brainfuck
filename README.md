# Haskell Brainfuck
A little Brainfuck Interpreter written in Haskell in order to practice Monad Transformers.

* `>` - move the pointer right
* `<` - move the pointer left
* `+` - increment the current cell
* `-` - decrement the current cell
* `.` - output the value of the current cell
* `,` - **replace** the value of the current cell with input
* `[` - jump to the **matching** `]` instruction if the current value is zero
* `]` - jump to the **matching** `[` instruction if the current value is **not** zero