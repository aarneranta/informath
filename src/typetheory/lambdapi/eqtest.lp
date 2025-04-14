constant symbol Set : TYPE;
constant symbol Elem : Set → TYPE;
constant symbol Equal : Π [A : Set], Elem A → Elem A → TYPE;
constant symbol Refl : Π [A : Set], Π (a : Elem A), Equal [A] a a;
constant symbol N : Set;
constant symbol 0 : Elem N;
constant symbol R : Set;
constant symbol pi : Elem R;
symbol eq0 : Equal 0 0 ≔ Refl 0;
symbol eqpi : Equal pi pi ≔ Refl pi;

