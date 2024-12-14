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
- each transition can be formatted as: `<current_state> <input_symbol> <stack_top> <push_symbol(s)> <next_state>`
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
"a" "b"
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

Following this, our data structure looks like:
```
q_0 q_1
q_0
q_1
S P F O C N D T H
"just put the" "in the bag bro" "pause" "and" "ohio" "pluh" "rizz" "mewer" "mewing" "twin" "unc" "skibidi" "toilet" "hawk" "tuah" "ahh" 
q_0 \ \ S q_1
q_1 "just put the" "just put the" \ q_1
q_1 "in the bag bro" "in the bag bro" \ q_1
q_1 "pause" "pause" \ q_1
q_1 "and" "and" \ q_1
q_1 "ohio" "ohio" \ q_1
q_1 "pluh" "pluh" \ q_1
q_1 "rizz" "rizz" \ q_1
q_1 "mewer" "mewer" \ q_1
q_1 "mewing" "mewing" \ q_1
q_1 "twin" "twin" \ q_1
q_1 "unc" "unc" \ q_1
q_1 "skibidi" "skibidi" \ q_1
q_1 "toilet" "toilet" \ q_1
q_1 "hawk" "hawk" \ q_1
q_1 "tuah" "tuah" \ q_1
q_1 "ahh" "ahh" \ q_1
q_1 \ S PF q_1
q_1 \ P O q_1
q_1 \ P OCP q_1
q_1 \ P \ q_1
q_1 \ F "just put the"O"in the bag bro" q_1
q_1 \ F \ q_1
q_1 \ O "ohio" q_1
q_1 \ O "pluh" q_1
q_1 \ O "rizz" q_1
q_1 \ O "mewer" q_1
q_1 \ O "mewing" q_1
q_1 \ O "twin" q_1
q_1 \ O "unc" q_1
q_1 \ O DO q_1
q_1 \ O N q_1
q_1 \ C "and" q_1
q_1 \ N "skbidi"T q_1
q_1 \ N H"tuah" q_1
q_1 \ D D"ahh" q_1
q_1 \ D N"ahh" q_1
q_1 \ D CD q_1
q_1 \ D "glaze" q_1
q_1 \ D "cooked" q_1
q_1 \ D "sus" q_1
q_1 \ D "mid" q_1
q_1 \ D "crashout" q_1
q_1 \ D \ q_1
q_1 \ T "toilet" q_1
q_1 \ T \ q_1
q_1 \ H "hawk" q_1
q_1 \ H \ q_1
```
