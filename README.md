# 510 proj - brainrot lang
example run 
```haskell
:l cfgparser.hs 
let auto = outAutomaton 
simulate auto ["hawk", "tuah", "and", "sus", "and", "crashout", "ohio", "lambda"]
```

## Automata Data Structure
Our specification of this language follows the format below:
- lists are space-delimited
- the file begins with a list of potential states (line 1)
- start state (line 2)
- list of possible accepting states (line 3)
- stack alphabet Γ as a list (line 4)
- the list of terminal strings (input alphabet Σ) (line 5)
    - terminal strings use double quotes (to facilitate whitespace)
- each transition can be formatted as: `<current_state> <input_symbol> <stack_top> <push_symbol> <next_state>`
- lambda is included by default in both alphabets and called by `\` Haskell-style

Let's look at an example from class. Here's the grammar:
S -> aXS | bYS | λ
X -> aXX | bYX | b
Y -> aXY | bYY | a

the original PDA looks like:
```
q_0 q_1
q_0
q_1
S X Y
q_0 \ \ S q_1
q_1 "a" S XS q_1
q_1 "a" X XX q_1
q_1 "a" Y XY q_1
q_1 "b" S YS q_1
q_1 "b" X YX q_1
q_1 "b" Y YY q_1
q_1 "a" Y \ q_1
q_1 "b" X \ q_1
q_1 \ S \ q_1
```

