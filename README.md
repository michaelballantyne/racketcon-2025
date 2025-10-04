# Advanced Macrology:  5 Macro Programming Patterns You (No Longer) Need to Know

`syntax-spec` abstracts over certain macro programming patterns.

Let’s build up to code like syntax-spec generates for you, to see how it works.
     *And, why you don’t want to write it by hand*

Our goal: create a macro layer over a DSL that...

 * Enforces a grammar
 * Checks name binding
 * Performs a compiler optimization
 * Could support macro extensions
