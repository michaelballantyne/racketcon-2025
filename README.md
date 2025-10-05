# Advanced Macrology:  5 Macro Programming Patterns You (No Longer) Need to Know

`syntax-spec` abstracts over certain macro programming patterns.

Let’s build up to code like `syntax-spec` generates for you, to see how it works.
     *And, why you don’t want to write it by hand*

Our goal: create a macro layer over a DSL that...

 * Enforces a grammar
 * Checks name binding
 * Performs a compiler optimization
 * Could support macro extensions

We'll look at four strategies: macro embedding, recursive rewriting and a DSL-specific macro expander... by hand, and with syntax-spec.



# Conclusion

### Macro embedding
Individual macros abstract over syntactic patterns, but can't implement complex binding structure or optimizations.
### Recursive rewriting
A recursive macro can check grammar and realize DSL binding structure with macro bindings, but doesn't get the opportunity to optimize after.
### A DSL macro expander
A DSL-specific macro expander can reuse the elements of Racket's expander, but you have to combine them correctly---a major challenge.
### syntax-spec`
25 lines of high-level spec replaces 100 lines of DSL expander code, and provides tools for your compiler to ask questions about DSL syntax.