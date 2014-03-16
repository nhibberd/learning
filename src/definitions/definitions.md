Referential transparency and purity
===================================
An expression `e` is _referentially transparent_ if for all programs `p`, all occurrences of `e` in `p` can be replaced by the result of evaluating `e` without affecting the meaning of `p`. A function `f` is _pure_ if the expression `f (x)` is referentially transparent for all referentially transparent `x`.
[Functional Programing in Scala](http://www.manning.com/bjarnason/)

Variance (Co/intra)
===================
