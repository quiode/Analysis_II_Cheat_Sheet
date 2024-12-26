
// global configuration
#set page(
  flipped: true,
  numbering: "1/1",
  columns: 3,
  paper: "a4",
  margin: 1cm,
  footer: context [
    #grid(
      columns: (1fr, 1fr, 1fr),
      align: (left, center, right),
      [Source: #link("https://gitlab.dominik-schwaiger.ch/quio/Analysis_II_Cheat_Sheet", "gitlab.dominik-schwaiger.ch")],
      [#counter(page).display(
          "1 of 1",
          both: true,
        )],
      [
        Version: #datetime.today().display("[day].[month].[year]")
      ],
    )
  ],
)

#set document(
  author: "Dominik Schwaiger",
  keywords: ("Spick", "Analysis", "Analysis II", "ETH", "Prüfung", "Exam", "Cheat Sheet"),
  title: "Analysis II Cheat Sheet",
)

#let def(body) = block(body, stroke: blue, inset: 4pt, radius: 2pt)
#let lem(body) = block(body, stroke: green, inset: 4pt, radius: 2pt)
#let form(body) = block(body, radius: 2pt, fill: gray, inset: 4pt)
#let limit = $attach(lim, tr: x -> x_0, br: x != x_0)$

= Analysis II Cheat Sheet

== Continuity in $RR^n$

Let $x_0 in X subset.eq RR^n$ and $f: X -> RR^m$

=== Convergence

#def()[
  ==== Definition

  Let $(x_k)_(k in NN)$ where $x_k in RR^n$. Write $ x_k = (x_(k,1), ..., x_(k,n)) $
  Let $y = (y_1, ..., y_n) in RR^n$. We say that the sequence $(x_k)$ *converges* to $y$ as $k -> + infinity$ if #form()[$ forall epsilon > 0: exists N >= 1: forall k >= N: ||x_k-y|| < epsilon $]
]

#lem()[
  ==== Lemma

  The sequence $(x_k)$ converges to $y$ as $k -> + infinity$ iff#footnote("if and only if")one of the following *equivalent* conditions holds:
  + For each $i$, $i <= i <= n$, the sequence $(x_(k,i))$ of real numbers converges to $y_i$.
  + The sequence of real numbers $||x_k - y||$ converges to $0$ as $k -> + infinity$.
]

=== Continuity

#def()[
  ==== Definition

  + We say that $f$ is *continuous at $x_0$* if $forall epsilon > 0: exists delta > 0: forall x in X:$ #form()[ $ ||x - x_0|| < delta ==> || f(x) - f(x_0) || < epsilon $ ]
  + We say that $f$ is *continuous on $X$* if it is continuous at $x_0$ for all $x_0 in X$
]

#lem()[
  ==== Proposition

  The function $f$ is continuous at $x_0$ iff, for every sequence $(x_k)_(k >= 1)$ in $X$ sucht that $x_k -> x_0$ as $k -> + infinity$, the sequence $(f(x_k))_(k >= 1)$ in $RR^m$ converges to $f(x)$.
]

=== Limit

Let $y in RR^m$.

#def()[
  ==== Definition

  We say that $f$ has the *limit* $y$ as $x -> x_0$ with $x != x_0$ if $ forall epsilon > 0: exists delta > 0: forall x in X: x != x_0 : $
  $
    #form()[$
    ||x - x_0|| < delta => || f(x) - y || < epsilon
  $]
  $
  We then write $ #form()[$ limit f(x) = y $] $
]

#lem()[
  ==== Proposition

  We have $ limit f(x) = y $ iff, for every sequence $(x_k)$ in $X$ sucht that $x_k -> x_0$ as $k -> + infinity$, and $x_k != x_0$, the sequence $(f(x_k))$ in $RR^m$ converges to $y$.
]


#lem()[
  ==== Proposition
  Let $Y subset.eq RR^m$ and $p >= 1$ an integer. Let $f: X -> Y$ and $g: Y -> RR^p$ be continuous functions. Then the composite *$g space.thin circle.small space.thin f$* is continuous.
]

=== Bounds

#def()[
  ==== Definition
  + A subset $x subset.eq RR^n$ is *bounded* if the set of $||x||$ for $x in X$ is bounded in $RR$.
  + A subset $X subset.eq RR^n$ is *closed* if for every sequence $(x_k)$ in $X$ that converges in $RR^n$ to some vector $y in RR^n$, we have $y in X$.
  + A subset $X subset.eq RR^n$ is *compact* if it is bounded and closed.
]

#lem()[
  ==== Proposition
  Let $f: RR^n -> RR^m$ be a continuous map. For any closed set $Y subset.eq RR^m$, the set
  $ f^(-1)(Y) = {x in RR^n: f(x) in Y} subset.eq RR^n $
  is closed.
]

#lem()[
  ==== Theorem
  Let $X subset.eq RR^n$ be a non-empty compact set and $f: X -> RR$ a continuous function. Then $f$ is bounded and achieves its *maximum* and *minimum*, or in other words, there exist $x_+$ and $x_-$ in $X$, such that
  $ f(x_+) = sup_(x in X) f(x), space.quad f(x_) = inf_(x in X) f(x) $
]

== Formula Collection

TODO
