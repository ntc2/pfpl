# Internal Notes

How far we've recorded feedback through:

N: ch 11
M: ch 11
K: ch 11
As a group: Ch 11
 
For mapping symbols back to latex, use
http://detexify.kirelabs.org/classify.html


# Legend

S = Suggestion

T = Typo

# Ch 4
S: p37, exercise 4.1: make an analogy between analytic mode <-> type
checking and synthetic mode <-> type inference.

# Ch 5
S: p44,45: explain that $\Gamma |- e \equiv e' : \tau$ parses as 
$\Gamma |- (e \equiv e') : \tau$ to avoid confusion about precedence.

# Ch 9

T: p72: in the premises for rules 9.5b and 9.5c, the premise for
typing $e_1$ is wrong: it should be $Gamma, x : nat, y : tau |- e_1 : tau$
in both rules.

T: p75: in exercise 9.6 it says to show that $e$ is hereditarily
terminating; it should be asking about $e'$.

# Ch 10

T: p81: the big \Pi concrete syntax in "The type prod[...], or \Pi_{i
\in I} tau_i" doesn't agree with the concrete syntax just given in
the Typ grammar higher up on the page. The Typ grammar uses
$\langle tau_i \rangle_{i \in I}$.

T: p83, exercise 10.1: same as previous typo.

# Ch 11

T: p87: analogous to typo mentioned above on p81 in Ch 10, there are
two different notations for sum types.

S: p87: rule 11.4d has $i \cdot e \val$ in premise, but $[ e \val ]$
would be more consistent with the rest of the book; both versions of
the rule are equivalent though.

S: p90: instead of "which", use "ifnull" for both versions of the
syntax. The "which" seems arbitrary but the "ifnull" reminds us its
about null pointers.

# Ch 12

# Ch 24

T: p209, 211, 216: like in chapters 10 and 11, two different concrete
syntaxes are used for sums and products, but only one of them was ever
defined in the relevant syntax tables.

# Index

S: add symbols to the index, for example add $\triangleq$ to the index
and explain at first use (on page 9?) that it means "is defined as" or
"is shorthand for". Alternatively, add an appendix summarizing the
symbols, e.g. starting from the "cheat sheet" linked to from your web
page for the book.

S: add "admissibility" ref pages 21, 23

S: add "derivability" ref pages 21

S: add "mobile types" ref to pages 278, 279

S: add "kinds", ref page 154
