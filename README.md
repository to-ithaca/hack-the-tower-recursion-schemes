# hack-the-tower-recursion-schemes

## Catamorphism

A catamorphism takes in an algebra `F[A] => A`
It folds from the leaves up, replacing each node `F[A]` with `A`.

## Paramorphism

A paramorphism takes in `F[(Fix[F], A)] => A`.
It also folds from the leaves up, replacing each node `F[A]` with `A` and the fixed node `Fix[F]`.
So the `Fix[F]` provided is the fixed child node.

## Zygomorphism
???

## Prepromorphism

This folds from the leaves up, like a catamorphism, but also applies a natural transformation at each stage.  The result of the natural transformation is fed into the next step.  This can be used to discard branches of the calculation.

## Histomorphism
This has the signature `F[Cofree[F, A]] => A`.
Again, this folds up from the leaves and keeps track of the previously calculated value A.
TODO: a catamorphism does this too, so what does the cofree part give?
example uses odd / even elements of a list.
The cofree preserves the previously calculated elements in the recursive structure

## Anamorphism
This has the signature `A => F[A]`.
It repeatedly applies `A` and maps over `F[A]` until `F[A]` terminates.

## Apomorphism
This goes from `A => F[Either(Fix[F], A)]`

## Postpromorphism

## Haskell concepts

Composing / splitting catamorphisms
Fixedpoint typeclass
Cofix
Sliding window using para
para for cataTrace
free monad for string contexts
Annotation with Cofree, top down, bottom up
cataM and monad transformers
zygomorphism - for 
