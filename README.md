A parser generator library I wrote for my 3rd year dissertation.

Will parser grammar files in BNF or EBNF notation, or you can write it using the combinators defined in Parser.Primitives.

BNF grammars (and EBNF after they've been converted with Parser.EBNFtoBNF) can be turned into a parser using Parser.GeneratorBNF
or a haskell file which uses the combinators to export such a parser with Parser.HSGeneratorBNF.