# Internal Notes

How far we've recorded feedback through:

N: ch 17
M: ch ??
K: ch ??
As a group: Ch 17
 
For mapping symbols back to latex, use
http://detexify.kirelabs.org/classify.html


# Legend

E = Error

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

T: p100: in rule 12.9 the existing premises are wrong and should
instead be "\Gamma |- p : \phi_1 \wedge \phi_2"

T: p100: rule 12.10a the second premise is wrong and should 
be "\Gamma |- p_1 : \phi_1"

# Ch 13

T: p107: "where $m \ge 0$, in which the variables $u_1,\ldots,u_n$
stand" should have "$u_m$" instead of "$u_n$"

S: p111: reword "All proofs in classical logic proceed by
contradicting the assumption that it is false." The "it is false" here
is confusing. What does "it" refer to?

# Ch 15

E: p126: the "recursor" $rec_{nat}$ given by rules 15.2b,c is more
akin to the "iterator" of Ch 9 than the "recursor" of Ch 9, because
the "body" $x.e_1$ of $rec_{nat}$ receives the recursive call in $x$
but does not receive the predecessor. So, Exercise 15.2 doesn't make
sense, and neither does calling $rec_{nat}$ the "recursor".

S: p128: in the $Typ$ grammar in 15.2.1 many type formers are omitted,
e.g. $sum(\tau_1; \tau_2)$. Mention that they are omitted. In other
words, say that the given $Typ$ is implicitly an extension of the
$Typ$ grammar for $T$.

T: p131: remove "to" in "or to the successor of some other".

S: p131: the paragraph starting "Intuitively speaking" is hard to
read.  It relates to 15.8a, where "every value of an inductive type"
refers to the value and type on the conclusion of 15.8a, and the
"value of the unfolding of the inductive type" refers to the premises
of 15.8a.  Being able to see this relationship between the
mathematical definition and the text helps to understand
both. Similarly, the discussion of coninductive unfoldings here is
about Rule 15.8c. It would probably be more clear if you referenced
those rules explicitly (parenthetically?) in this paragraph

# Ch 17

T: p147: "covnention" should be "convention".

E: p149, p151: on p149 the implementations of the queue type don't
have the $opt$ in their type (i.e. see $e_r$ which implements $rem$,
has wrong type on p149 and p150), but on p151 they do. Ch 18 has the
$opt$ consistently, so everything should be fixed to have $opt$.

T?: p152: should the $\cong$ here be $\equiv$ instead?
I.e. definitional equality instead of isomorphism.

T: p152: in the definition of $rem$ the $\langle bs, fs' \rangle$
should be wrapped in $just(\langle f, <here> \rangle)$.

# Ch 20

S: p172: the understanding of the dynamics for FPC would be helped by
noting that they can be compared to \bold{M} in Chapter 15, which
allowed inductive/coinductive types but which were total.  The
difference between \bold{M} and \bold{FPC} is that \bold{M} restricted
\it{t} to the positive position, but \bold{FPC} allows \it{t} in the
negative position, therefore rendering \bold{FPC} partial.

# Ch 21

T: p181: should rule 21.2a actually be:

        \Gamma |- u ok
        --------------
        \Gamma |- u \equiv u

   to state that u is equivalent only if well formed (i.e. move the
   judgement in the conclusion to the premises)?

# Ch 24

T: p209, 211, 216: like in chapters 10 and 11, two different concrete
syntaxes are used for sums and products, but only one of them was ever
defined in the relevant syntax tables.

# Ch 28

T: p259 the sentence "let e' be such that k;ap(lam{\tau}(x.e1); -)
\bowtie e2 = e'" is a call-by-value dynamic, whereas the prior
definitions (specifically 28.5c) is call-by-name.  The call-by-name
definitions for p259 would be

    --------------------------------------------------------------------------------
    k;ap(-;e2) \ltriangle lam{\tau}(x.e1) |-> k;ap(lam{\tau}(x.e1); -) \rtriangle e2


    ----------------------------------------------------------------
    k;ap((lam{\tau}(x.e1);-) \ltriangle e2 |-> k \rtriangle [e2/x]e1

# Ch 30

T: p267: "To take another example, given that k has type \tau cont and
f has type \tau' -` \tau, return k' of type return a continuation k'
of ..."  remove the "return k' of type".

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
