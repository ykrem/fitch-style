#import "lib.typ": proof
#import "src/formula.typ": start, end, assume
#import "src/framing.typ": framing

= Examples

== De Morgan: $not(p and q) tack (not p or not q)$.
Proof (dynamic mode): 
#proof(assumption-mode: "dynamic", (
$not(p and q)$,
assume,
start,
  $not(not p or not q)$,
  assume,
  start,
    $not p$,
    assume,
    ($(not p or not q)$, $or I quad 3$),
    ($tack.t$, $tack.t I quad 2,4$),
  end,
  ($not not p$, $not I quad 3-5$),
  ($p$, $not E quad 6$),
  start,
    $not q$,
    assume,
    ($(not p or not q)$, $or I quad 8$),
    ($tack.t$, $tack.t I quad 2,9$),
  end,
  ($not not q$, $not I quad 8-10$),
  ($q$, $not E quad 11$),
  ($(p and q)$, $and I quad 7,12$),
  ($tack.t$, $tack.t I quad 1,13$),
end,
($not not(not p or not q)$, $not I quad 2,14$),
($(not p or not q)$, $not E quad 15$),
))

#pagebreak()

== Some Natural Deduction Rules

#let and-intro = proof((
  ($m$, $p$, $$),
  ($n$, $q$, $$),
  ($o$, $(p and q)$, $and I quad m,n$)
))

#let and-elim = proof((
  ($m$, $(p and q)$, $$),
  ($n_1$, $p$, $and E quad m$),
  ($n_2$, $q$, $and E quad m$),
))

#let or-intro = proof((
  ($m$, $p$, $$),
  ($n$, $(p or q)$, $or I quad m$)
))

#let or-elim = proof(indexation: "a", (
  $(p or q)$,
  start,
    $p$,
    assume,
    $r$,
  end,
  start,
    $q$,
    assume,
    $r$,
  end,
  ($r$, $or E quad a, b-c, d-e$)
))

#stack(
  dir: ttb,
  spacing: 5em,
  stack(dir: ltr, spacing: 15em, and-intro, and-elim),
  stack(dir: ltr, spacing: 15em, or-intro, or-elim)
)

// note how (it seems as) the ones with paren. are further to the right. Idk if true, but consider.

#pagebreak()


== Excluded Middle: $tack (p or not p)$

#proof((
start,
  $not (p or not p)$,
  assume,
  start,
    $p$,
    assume,
    ($(p or not p)$, $or I quad 2$),
    ($tack.t$, $tack.t I quad 1,3$),
  end,
  ($not p$, $not I quad 2-4$),
  ($(p or not p)$, $or I quad 5$),
  ($tack.t$, $tack.t I quad 1,6$),
end,
($not not (p or not p)$, $not I quad 1-7$),
($p or not p$, $not E quad 8$)
))