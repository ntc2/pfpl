# Internal Notes

How far we've recorded feedback through:

N: ch 30 p267
M: ch ??
K: whole book
As a group: Ch 17
 
For mapping symbols back to latex, use
http://detexify.kirelabs.org/classify.html

# Legend

E = Error

S = Suggestion

T = Typo

Q = Question (perhaps a euphemism for "I'm worried this doesn't make
    sense")

# Markup

We've used pseudo Latex for math; it's certainly not correct Latex, so
hopefully the intent will clear.

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

# Ch 18

T: p154: the type $emp$ in the queue example is wrong, it should be
$\forall t :: T. q[t]$

T: p154: "an implementation of the existential given in the preceding
paragraph *have* to give": the "have" should be "has".

S: p154: it's not clear what "accounts for" means in "The extension
accounts for definitional equality of constructors". You follow with
an example, but a general description of what you mean would also be
good. Perhaps something along the lines of "The extension includes a
notion of \"definitional equality\" of constructors, meaning that it
defines when two types $\tau$ and $\tau'$ are equal up to type-level
computation (see Section 18.2), and ensures that $\Gamma |- t : \tau$
implies $\Gamma |- t : \tau'$ whenever $\tau$ and $\tau'$ are
definitionally equal types (see Rule 18.3)."

S: p156: remind us what "the three constants" are again, since
e.g. it's natural to think of the unit type $\langle \rangle$ as a
"constant", but it's not one of the three constants you're referring
to.

# Ch 19

S: p161: tell us where the name PCF comes from ("partial computable
functions"?)

S: p165: reword "Recursive functions are defined in PCF using
recursive functions" to "Recursive functions are defined in PCF using
fixpoints"

S: p166: when defining the minimization operator $\phi$, tell us what
kind of function it is (primitive recursive?). Also, we've been
talking about $\mathbb{N} \partial \mathbb{N}$ functions, and now
you've introduced a $\mathbb{N} x \mathbb{N} \partial \mathbb{N}$
function. Perhaps a footnote pointing out that they're equivalent
would be helpful, i.e. pointing out that encoding and decoding between
nats and pairs of nats is possible here.

S: p166: in the proof sketch for Theorem 19.3, say that the "evaluator
for expressions of PCF" is the thing you later call $\phi_{univ}$ two
paragraphs farther down.

T: p166: in "given the code $\bar{\godel{e}}$ of a closed expression"
the "$\bar{\godel{e}}$" should be "$\godel{e}$, since $\phi_{univ}$ is
defined on numbers $\mathbb{N}$, not expressions of type $nat$.

S: p167, top of page: explain that $e_{univ}$ is the PCF
implementation of $\phi_{univ}$,
i.e. $e_{univ}(\bar{\godel{e}})(\bar{m}) \equiv e(\bar{m})$ forall $e$
in PCF and $m,n \in \mathbb{N}$.

S: p168: give a citation for "Blum Size Theorem".

T: p169, Exercise 19.5: "function specified [as] a function of two arguments", the "as" is missing.

S: p169, Exercise 19.5: be clear that $e$ is total, or explain that
it's not. In other words, clarify that $e$ converges even if *both*
arguments diverge.

# Ch 20

S: p171, intro: tell us what "FPC" stands for.

S: p171, bottom of page: when you say "inherited from the preceding"
development, tell us which development. In particular, it's not clear
if "FPC" is general recursion or not;in Section 20.3 we learn that
recursive types allow us to derive general recursion, but it's not
clear if it's also inherited.

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

T: p187, top: missing parens on both applied lambdas. the
$\lambda(x)u_1(u_2)$ should be $\left(\lambda(x)u_1\right)(u_2)$, and
similar for the other application.

# Ch 22

T: p195, top: in the abstract syntax of the first expression, the
$ifz$ should be applied to $y$ but is not. i.e. insert $(y)$ between
the last $\}$ and last $)))$.

# Ch 24

S: p209, middle of page: remove ", other than bind it to a variable,"
and replace "operation" with "elimination". There are many
"operations" we can perform besides binding to a variable, e.g. we can
use the value to build up a bigger value, or apply a function to it,
or ...

T: p211, top: add "is" in "A type constructor [is] covariant in an
argument".

T?: p214, bottom: the "\tau :: T" premise on Rule 24.15b seems out of
place / redundant, since the system we're studying doesn't have higher
kinds. The intro to this subsection earlier on the page says we're
extending the systems from chapters 16 and 17, but higher kinds were
introduced in Ch 18.

T: p209, 211, 216: like in chapters 10 and 11, two different concrete
syntaxes are used for sums and products, but only one of them was ever
defined in the relevant syntax tables.

# Ch 25

T: p219, middle: replace "a" with "as" in "serve *a* behavioral
specifications of expressions".

T: p221, bottom: add "at" in "the conjunction $\num ! \phi_1$ and
$\fun ! \phi_2$ entails any refinement [at] all".

T: p223: the premise of Rule 25.4d should have "$e \elem_{dyn}$"
instead of "$e \elem_{dyn -> dyn}$".

T: p224: in the conclusion of Rule 25.5d should have $\phi_2$ instead
of $\phi_1$.

S?: p225, bottom: you say "we may combine rules (25.3b) and (25.4a) to
derive the judgment $num ! \bar{n} \elem_{dyn} \top$", but doesn't
this follow directly from rules (25.4a) and (25.8a,b), and so
subsumption is not relevant here?

T: p228, middle: missing "prove" in "we may [prove] type preservation
and progress".

# Ch 28

E: p255,p258,p259: on p259 the sentence "let e' be such that
k;ap(lam{\tau}(x.e1);-) \bowtie e2 = e'" is a call-by-value dynamic,
whereas the prior definitions (specifically Rule 28.5c) is
call-by-name.  The missing call-by-value rules needed for p259 to make
sense could be

* on p255, as part of the 28.5 rules:

        ------------------------------------------------------
        k;ap(-;e2) \ltriangle e1 |-> k;ap(e1; -) \rtriangle e2

        ----------------------------------------------------------------
        k;ap((lam{\tau}(x.e1);-) \ltriangle e2 |-> k \rtriangle [e2/x]e1

* on p258, as part of the 28.12 rules:

        k \bowtie ap(e1;e2) = e
        -------------------------
        k;ap(e1;-) \bowtie e2 = e

# Ch 29

T?: p261, Rule 29.3a: shouldn't the rule be

    -----------------------------
    \epsilon \ltriangle e initial

S?: p161, bottom: add an exercise concerned with defining the
extension mentioned in "the definition of stack typing given in Ch 28
can be extended". However, this might already be part of Exercise
29.1?

Q?, p262, middle: it says "we use a by-value interpretation to avoid
the problem of imprecise exceptions that arises under a by-name
interpretation". Perhaps this is related to by-name/by-value confusion
in the previous chapters on p255,258,259?

Q?, p263, Rule 29.6c: why no "e val" premise on Rule 29.6c? Or
equivalently, why have the "e val" premise on Rule 29.6b? I expect
either both or neither, but not half.

Q?, p265, exercises 29.4, 29.5: what's $MPCF$? Is it defined in the
book somewhere?

# Ch 30

T: p267: "To take another example, given that $k$ has type $\tau cont$
and $f$ has type $\tau' \rightharpoonup \tau$, *return $k'$ of type*
return a continuation $k'$ of ..."  remove the "return $k'$ of type".

T: p268, Rule 30.2: the premise is $k : \tau$ but should be $k
\vartriangleleft : \tau$.

T: p269, Lemma 30.1: like the last typo, the $k : \tau$ in the
conclusion should be $k \vartriangleleft : \tau$.

T?: p270: I worry you don't use the terms "routine" and "coroutine"
consistently here. For example, the type $rout$ is called "coroutine",
as is the type $coro$. But I think there are other inconsistencies
too, but rereading it now I find it too confusing to be sure.

S: p273: reference Exercise 13.1 in Exercise 30.2.

# Ch 31

S: p277, 2nd paragraph: explain that "renamed at will" for symbols
here means that symbols will be renamed *dynamically* at run time. In
other words, $new$/$decl$/whatever symbol primitive creates *fresh*
symbols at run time in the scope free dynamics. A lot of this dynamic
renaming happens in Ch 36, but we didn't realize that here in Ch 31
and got confused later.

S: p279, definition of "mobility condition" near the top: better to
state the definition in the equivalent but simpler style of Ch 34,
i.e.

    if tau mobile and |-_{Sigma} e : tau and e val_{Sigma}
    then |-_{emptyset} e : tau and e val_{emptyset}


S: p279, beginning of section 31.1.2: remind the reader that all types
are mobile in the scope free dynamics.

Q/S: p279, state notation for scope free dynamics: why $nu$ in $\nu
\Sigma \{ e \}$? If there's a mnemonic here then explain it.

S: p279, bottom: following up on the p277 suggestion, tell the reader
that the scope-free dynamics require *dynamic* fresh symbol generation
and renaming, and give an example. For example, evaluation of

    let n = new{nat} a.0 in (n,n)

requires dynamic symbol generation and renaming for $a$ in evaluating
the right occurrence of $n$ in the tuple $(n,n)$. (The rules 36.3c and
36.3h include explicit dynamic symbol generation, but our confusion
was around dynamic generation + renaming.)

E: p283, exercise 31.5: the $MPCF$ here that you claim is described in
Ch 29 is actually only mentioned in an exercise in Ch 29, and even
there seems to be a "dangling" reference. It seems you must have
removed the section that describes this language.

# Ch 32

S: p285, middle: explain that the "big dot" in $a \hookarrow <big
dot>$ is being defined here / we aren't supposed to have seen it
earlier and already know what it means.

T: p288, bottom: in "the choice of *fluid* on which a $\get$ or $\put$
acts" the "fluid" should be "symbol".

# Ch 33

# Ch 34

S: p305, top: remove the definition of the judgment $m || \mu
ok_{\Sigma}$. It's never used. You could use it in the statement of
Theorem 34.1, but I think it's better to just delete the definition.

S: p306, middle: call the derived $if$ here $ifz$ (or $ifzero$ if you
don't want to "abuse notation"), like the underlying
primitive. Calling it $if$ is confusing, because its semantics are the
opposite of $if$ in common real world languages (e.g. C, Python),
where zero is false and all other values are true. (I guess zero is
true in common shells, e.g. sh, bash, zsh, but I expect most people
are more familiar with the opposite convention in C and Python).

E: p310, Exercise 34.2: the "fix" expression doesn't make sense: $p$
is used both as the recursive binding in $fix p$ and the argument
binding in $\lambda(p : \tau)$. Not sure what's intended here.

# Ch 35

T: p313, end of first paragraph: missing "same" in "any two references
may refer to the [same] assignable".

S: p313, end of first paragraph of Section 35.1: the last sentence is
confusing with two uses of "any given assignable" not referring to the
same given assignable. A possible rewording is "We can write such a
procedure for any [specific] assignable, $a$, but what if we wish to
write a generic procedure that works for [all assignables
uniformly]?".

T: p313, bottom: missing "to" in "One way to do this is [to] give the
procedure".

T: p314, top: the definition of $tau cap$ has $nat$ in place of $tau$. I.e. it should be $tau cap \triangleeq = tau cmd x (tau \partial tau cmd)$.

S: p318, beginning of Section 35.4: you should forward reference the
"back patch" example in Section 35.5, when giving the example code
that presumably uses back patching to set the reference $a$.

T: p318: the "mu : Sigma" in "Cyclic dependencies complicate the
judgment *mu : Sigma*." should be "nu Sigma \{ m || mu \} ok".

S: p318: in the definition of $|-_{Sigma'} mu : Sigma$, say more about
the reason for the two different symbol contexts $Sigma$ and $Sigma'$.

T: p319, proof of Theorem 35.2: you say "For the second statement, we
prove the stronger form <stronger form>", but the "stronger" form is
not any stronger. Rather, it's identical after unfolding the
definition of $nu Sigma \{ m || mu \} ok$.

# Ch 36

T: p323, 2nd to last and last paragraphs: the "by name" should be "by
need" instead, in "Lazy languages adopt the opposite strategy,
preferring a *by name* dynamics for functions" and in "called LPCF, in
which functions are called *by name*".

S: p324, top: reorder the sentences describing "by-name" and "by-need"
to describe "by-need" first, and then explain "by-name" as a
contrast. I.e., instead of "By-name function application replicates
the unevaluated argument by substitution, which means that there can
arise many copies of the same expression, each evaluated separately,
if at all. By-need evaluation uses a device called /memoization/ to
share all such copies of an argument and to ensure that if it
evaluated at all, its value is stored to so that all other uses of it
will avoid recomputation.", try something like "By-need evaluation
uses a device called /memoization/ to share all such copies of an
argument and to ensure that if it evaluated at all, its value is
stored to so that all other uses of it will avoid recomputation. In
contrast, by-name function application, which we saw in earlier
chapters, replicates the unevaluated argument by substitution, which
means that there can arise many copies of the same expression, each
evaluated separately, if at all."

S: p324, second paragraph: insert "is a memo table that" into "and mu
[is a memo table that] maps", to make it more clear how "mu" relates
to the mention of "memoization" and "memo table" in the previous
paragraph.

S: p324, end of second paragraph: get rid of "$via a$", the abstract
syntax for "$@a$". You don't use it in any rules and only ever mention
it again when repeating that it's abstract syntax.

T: p326, 4th paragraph: missing "@" in "the expression $fix x:tau is
x$ associates the expression $[@]a$ to $a$".

T: p328, proof of theorem 36.2: you say "Consider Rule (19.1a)". That
is surely not the rule you intend; I'm not sure which you rule you
meant.

S: p330, top: add big parens in "$rec t is [\left(](unit + (nat x t))
susp [\right)]$.

S: p330, middle: extend the sentence "The expression $lcell[a]$ is a
reference to the suspension named $a$" by adding "[, which is
introduced when $a
susp$ is evaluated (distinct from "forced")]." at the end.

T: p332, exercise 36.4: missing hats on all LHSs of translations.

S?: p332: you could add a new exercise about recovering lazy control
operators (if-then-else, when, unless) in SFPC. Of course, $when$ and
$unless$ are probably only useful with side effects, so perhaps the
exercise should be to recover them in MA? Possible solution is to use
macros, e.g.

    when c e => let s = susp _ is e
                in if c then force s else ()

# Ch 37

T: p336, bottom: missing "s" in "the static[s] of PPCF enriches".

S: p338, top: add diagrams illustrating the cost graph grammar
components.

T: p339, middle: twice you say "$\let$" when you mean "$\par$" or
"parallel let".

T: p339, middle: missing "we" in "and [we] assign unit cost to the
substitution".

S: p339, middle: you say "and [we] assign unit cost to the
substitution, because we expect it to be implemented by a
constant-time mechanism for updating the environment". This sounds
nice, but assigning unit cost is also required by rules 37.2c and
37.3d if we ant to prove Theorem 37.6. Make this connection/compulsion
more explicit.

T: p344, 3rd paragraph: replace "mapping" with "map from" in "and mu
is a finite *mapping* the task names in Sigma".

Q: p344, rules 37.10a and 37.10b: why not just have simpler rules like

    e |-> e'
    --------------------------------------------------------------
    nu a \{ a \hookarrow e \} |->_{loc} nu a \{ a \hookarrow e' \}

where "$e |-> e'$" is the dynamics for non threaded underlying
expression language? It seems weird to fork threads here but no allow
any parallelism (i.e. these rules don't allow evaluating a function
and its argument at the same time).

E: p345, Rule 37.10c: with your current rules 37.10a and 37.10b, the
rule 37.10c is wrong (the substitution in the conclusion makes no
sense). The premise for $e1$ should be "$e1 = e1'(x2) or e1 =
x2(e1')$" and in the conclusion the "$x2.e1(x2)$" should be
"$x2.e1$". I.e., make it a single rule that complements both 37.10a
and 37.10b.

T: p345, just below Rule 37.10c: "create *create*".


# Ch 38

S: p350, intro: add a reference to Section 36.4 for "suspensions".

Q: p350, 4th paragraph: my reading of the chapter disagrees with this
summary of the trade offs between futures and speculations. My reading
is that, for the parallel dynamics, a program using futures will
always take *at least* as many local steps as the corresponding
program using speculations. I.e., speculations seems to be more
efficient than futures.

Q: p352, Section 38.2.2: it doesn't seem true that the "sequential
dynamics [of] speculations are simply a reformulation of
suspensions". For example, the following programs compute $e$ twice
speculations but only once for suspensions. Suspensions version:

    let x = susp _ : tau is e in force(x) + force(x)

Versus speculations version:

    let x = spec(e) in ssyn(x) + ssyn(x)

S: p352, bottom: add "and speculations" in "Futures [and speculations]
are only interesting insofar as".

S: p355, bottom: instead of using a footnote (4), call out inline that
you never defined "$let fut$".

T: p356, top: replaces "Elements of this type are suspended
computations" (plural) with "An element of this type is a suspended
computation" (singular), to agree with the rest of the sentence being
singular.

S: p356, top: be explicit about defining $mapforce$, i.e. add
"$mapforce(f)(s) \triangleeq = " before the definition.

# Ch 39

S: p361, bottom: explain that $P |--> P'$ is the top level/primary
evaluation of programs, and that $P |->^{alpha} P'$ is only an
auxiliary definition used to define $P |--> P'$. Perhaps it would help
to define the initial and final states for the top level evaluation,
but I'm not exactly sure what they should be. Perhaps

    |-_{emptyset} P proc
    --------------------
    P initial

    -------
    1 final

where "evaluating a program $P$" means

    P intial
    -----------
    P |-->_{emptyset}^* 1

I.e., evaluating a program means taking $|-->$ steps in an empty
symbol context.

S: p361, bottom: punning $|->^{epsilon}$ and $|->$ here is confusing
and unnecessary, so remove this "notational convenience". It's
particularly confusing because it seems you're using short and long
arrows to mean different things, and it's much harder to distinguish
the two arrows lengths without the subscript on the short arrow. In
fact, it would also help to use arrows that differ in more than just
their length, e.g. using a double arrow ($=>^{alpha}$) for the
$|->^{alpha}$ steps, with the mnemonic that the parallel horizontial
lines correspond to parallelism in the dynamics.

S: p364, Rule 39.11a: since the chapter is already using $alpha$ as a
meta variable standing for actions, remind the reader that here
$=_{alpha}$ means $alpha$-equivalence, and has nothing to do with
actions or meta variables.

T: p366, Rule 39.15d: the conclusion should use a long arrow
$|-->_{Sigma}$ (cf. Rule 39.4d).

T, Q: p366, bottom: replaces "types" with "terms" in "Variables range,
as always, over *types*". Also, why "always"? For example, variables
range over types in System F. Perhaps the point is that the variable
"$x$" always ranges over terms, whereas the variable "$t$" ranges over
types.

Q: p368, bottom third: re the section including "This means that
synchronous communication can be decomposed into a simpler
/asynchronous send/ operation", I don't understand what's
"asynchronous" about $asnd$ vs $snd$. Based on the dynamics, it seems
that $asnd[a](e)$ is equivalent to $snd[a](e;1)$, since both take a
$|-->_{Sigma}$ step to $1$ when $e$ is a value. If there is synchrony,
it must come from the dynamics. It seems that Rule 39.15d is
synchronous, /only if/ the evaluation of processes is defined to only
take $|->^{epsilon}$ steps (as in the presentation with initial and
final states outlined in my suggestion on p361). Without a restriction
like this, I don't see where synchrony comes from, or how Rule 39.19c
can make any sense, since if a process can take $|->^{?a(e)}$ steps
then it can consume values that were never produced!

Q: p369, Rule 39.21: why not require $alpha ~ tau$ here? Cf. Rule
39.19b.

T: p369, beginning of Section 39.6: missing "a" in "when one process
passes [a] /channel reference/".

E: p370, Rule 39.23b: don't need $e val_{Sigma, alpha ~ tau}$ in the
premise.

T: p371, Rule 39.23c: the $e val$ premise is nonsense; there is no $e$
in the conclusion.

# Ch 40

S: p380: explain why we need $wrap$. I.e., give an example explaining
why $bind$ and $sync$ are not sufficiently expressive.

# Ch 41

T: p386, top: can't both be "first" in "when the *first* is a sensible
counter-factual relative to the *first*".

S: p386, top: give both an example of a "sensible counter-factual" and
of a not "sensible counter-factual", and say more about what these
terms mean. I think your king example is supposed to be a not
"sensible counter-factual" example, but it's not very clear.

S: p389, rules 41.6*: don't omit nu, Sigma, and p in the rules. Using
a shorthand doesn't gain anything here, since we don't have long
proofs involving derivation trees or anything.

T: p390, theorems 41.2 and 41.3: you have $Sigma @ w$ below the arrow
and $a$ above, but it should be $Sigma$ below and $a @ w$ above.

S: p391, exercises: add an exercise about adding support for crashed
nodes to DA. Without he possibility of crashed nodes, it doesn't seem
very "distributed".

# Ch 42

S: p397, bottom: don't pun $x_i <- x_i$.

# Ch 43

T: p399, middle: add parens, replacing $Lambda(t)e[tau]$ with $\left( Lambda(t)e \right) [tau]$.

T: p399, bottom: the sentence starting "Singletons $n$ the type
definition problem" makes no sense. I don't know what you intended.

T: p400, middle: add parens around the type lambda to get $\left( Lambda (t :: S(t))e \right) [tau]$.

S: p400, middle: remind us that $T$ is the kind of types, and back
reference Ch 18. Also, tell us that $Type$ is a synonym for $T$ (used
e.g. in the 43.2* rules), or better yet, just pick one notation and
stick with it (we're not gaining anything here by jumping back and
forth between abstract and concrete syntax).

S: p402, middle: even after the examples, the singletons still seem
poorly motivated. You forward reference chs 44 and 45, but even
better, you could give more intuitive explanation and more examples
now.

T?: p404, Rule 43.10: perhaps there should be a substitution $[c_1/u]$
into $kappa_2$ in the conclusion?

# Ch 44

S: p410,411: the use of the term "module" is inconsistent, or at least
confusing. It seems to mean "sealed modules" sometimes, and
"structure" at other times. Clean this up / be consistent.

S: p411, bottom: add "distinct" in "two bindings of the same sealed
module result in two *distinct* abstract types".

S: p413, middle: you say "together with the covariance principle for
signatures", but I don't think you introduced this principle yet! I
think it's Rule 44.1c, so say so and make a forward reference.

S: p415, middle: you add value hypotheses $x val$ and $X val$ and then
elide them in all rules. Instead, why not add rules that say module
and expression variables are values, i.e.

    ----------------------
    Gamma, X:sigma |- X val

    ---------------------
    Gamma, x:tau |- x val

# Ch 45

S: p425, top: add "value" in "and $sigma^K_{dict}$ is the signature of
dictionaries of some [value] type $tau$ given as follows". I.e.,
clarify that $tau$ is the type of dictionary values, vs dictionary
keys or dictionaries as a whole.

T: p427, Rule 45.1a: the small $x$ in the conclusion should be a big
$X$.

# Ch 46

T: p435, top: remove "corresponds" in "equality of expression in a
functional language *corresponds* follows the familiar patterns".

T: P435, middle: replace "given" with "give" and "number" with
"numbers" in "Yet the two expressions are interchangeable because they
*given* the same result when applied to the same *number*."

# Ch 47

E: p448, top: you say that Corollary 47.17 implies that

    A'\{fix^m ...\} \simeq A'\{fix ...\}

However, Cor 47.17 does not go in the that direction (from a specific
bounded recursion $fix^m$ to unbounded recursion). Rather, Cor 47.17
says there exists $m'$ s.t.

    A'\{fix^m' ...\} \simeq A'\{fix ...\}

It seems you need a stronger version of Cor 47.17 here, that
incorporates Cor 47.15 (with an additional argument for the diverging
case) and says:

  There exists $M >= 0$ s.t. for all $m >= M$,

      [f^{(omega)}/y]e \simeq [f^{(m)}/y]e

With this stronger lemma, you can argue that there exist $M$ and $M'$ s.t. forall $m >= M$ and all $m' >= M'$ we have

        A\{fix ...\} \simeq A\{fix^m ...\}

and

        A'\{fix^m' ...\} \simeq A'\{fix ...\}

and then taking $n = max{M,M'}$ we get

        A\{fix ...\} \simeq A\{fix^n ...\}

and

        A'\{fix^n ...\} \simeq A'\{fix ...\}

which is what you were trying to establish.

T: p450, middle: replace $n$ with $n+1$ in "(in fact, $m = n$
suffices)", since e.g. $m = 0$ certainly does not suffice for $f(0)$
(it diverges).

E: p451, middle: same problem as on p448, where you use Cor 47.17
incorrectly.

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

S: add "call-by-name" and "call-by-value"
