
// CONFIGURATION
#set document(
  author: "Dominik Schwaiger",
  keywords: ("Spick", "Analysis", "Analysis II", "ETH", "Prüfung", "Exam", "Cheat Sheet"),
  title: "Analysis II Cheat Sheet",
)

#set text(size: 8pt)

#set par(spacing: 0.5em)

#set page(
  flipped: true,
  numbering: "1/1",
  columns: 3,
  paper: "a4",
  margin: (rest: 0.25cm, bottom: 0.75cm),
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

// VARIABLES

#let _block = block.with(inset: 4pt, radius: 2pt, width: 100%, breakable: true);
#let def(body) = _block(body, stroke: blue)
#let lem(body) = _block(body, stroke: green)
#let form(body) = _block(body, fill: gray)
#let limit = $attach(lim, tr: x -> x_0, br: x != x_0)$

= Analysis II Cheat Sheet

== Continuity in $RR^n$

Let $x_0 in X subset.eq RR^n$ and $f: X -> RR^m$

=== Convergence

#def()[
  Let $(x_k)_(k in NN)$ where $x_k in RR^n$. Write $ x_k = (x_(k,1), ..., x_(k,n)) $
  Let $y = (y_1, ..., y_n) in RR^n$. We say that the sequence $(x_k)$ *converges* to $y$ as $k -> + infinity$ if #form()[$ forall epsilon > 0: exists N >= 1: forall k >= N: ||x_k-y|| < epsilon $]
]

#lem()[
  The sequence $(x_k)$ converges to $y$ as $k -> + infinity$ iff#footnote("if and only if")one of the following *equivalent* conditions holds:
  + For each $i$, $i <= i <= n$, the sequence $(x_(k,i))$ of real numbers converges to $y_i$.
  + The sequence of real numbers $||x_k - y||$ converges to $0$ as $k -> + infinity$.
]

=== Continuity

#def()[
  + We say that $f$ is *continuous at $x_0$* if $forall epsilon > 0: exists delta > 0: forall x in X:$ #form()[ $ ||x - x_0|| < delta ==> || f(x) - f(x_0) || < epsilon $ ]
  + We say that $f$ is *continuous on $X$* if it is continuous at $x_0$ for all $x_0 in X$
]

#lem()[
  The function $f$ is continuous at $x_0$ iff, for every sequence $(x_k)_(k >= 1)$ in $X$ sucht that $x_k -> x_0$ as $k -> + infinity$, the sequence $(f(x_k))_(k >= 1)$ in $RR^m$ converges to $f(x)$.
]

=== Limit

Let $y in RR^m$.

#def()[
  We say that $f$ has the *limit* $y$ as $x -> x_0$ with $x != x_0$ if $ forall epsilon > 0: exists delta > 0: forall x in X: x != x_0 : $
  $
    #form()[$
    ||x - x_0|| < delta => || f(x) - y || < epsilon
  $]
  $
  We then write $ limit f(x) = y $
]

#lem()[
  We have $ limit f(x) = y $ iff, for every sequence $(x_k)$ in $X$ sucht that $x_k -> x_0$ as $k -> + infinity$, and $x_k != x_0$, the sequence $(f(x_k))$ in $RR^m$ converges to $y$.
]

#lem()[
  Let $Y subset.eq RR^m$ and $p >= 1$ an integer. Let $f: X -> Y$ and $g: Y -> RR^p$ be continuous functions. Then the composite *$g space.thin circle.small space.thin f$* is continuous.
]

=== Bounds

#def()[
  + A subset $x subset.eq RR^n$ is *bounded* if the set of $||x||$ for $x in X$ is bounded in $RR$.
  + A subset $X subset.eq RR^n$ is *closed* if for every sequence $(x_k)$ in $X$ that converges in $RR^n$ to some vector $y in RR^n$, we have $y in X$.
  + A subset $X subset.eq RR^n$ is *compact* if it is bounded and closed.
]

#lem()[
  Let $f: RR^n -> RR^m$ be a continuous map. For any closed set $Y subset.eq RR^m$, the set
  $ f^(-1)(Y) = {x in RR^n: f(x) in Y} subset.eq RR^n $
  is closed.
]

#lem()[
  Let $X subset.eq RR^n$ be a non-empty compact set and $f: X -> RR$ a continuous function. Then $f$ is bounded and achieves its *maximum* and *minimum*, or in other words, there exist $x_+$ and $x_-$ in $X$, such that
  $ f(x_+) = sup_(x in X) f(x), space.quad f(x_-) = inf_(x in X) f(x) $
]

== Partial derivatives

=== Openness

#def()[
  A subset $X subset.eq RR^n$ is *open* if, for any $x = (x_1, ..., x_n) in X$, there exists $delta > 0$ such that the set $ {y = (y_1, ..., y_n) in RR^n : |x_i - y_i| < delta "for all" i} $ is contained in $X$.

  In other words: any point of $RR^n$ obtained by changing any coordinate of $x$ by at most $delta$ is still in $X$.
]

#lem()[
  A set $X subset.eq RR^n$ is *open* iff the complement $ Y = {x in RR^n : x in.not X} $ is closed.
]

#lem()[
  If $f: RR^n -> RR^m$ is continuous and $Y subset.eq RR^m$ is open, then $f^(-1)(Y)$ is open in $RR^n$.
]

=== Derivative

#def()[
  Let $X subset.eq RR^n$ be an open set. Let $f: X -> RR$ be a function. Let $1 <= i <= n$. We say that $f$ has a *partial derivative* on $X$ with respect to the $i$-th variable, or coordinate, if for all $x_0 = (x_(0,1), ..., x_(0,n) in X)$ the function defined by $ g(t) = f(x_(0,1), ..., x_(0,i-1), t, x_(0, i+1), ..., x_(0,n)) $ on the set $ I = {t in RR: (x_(0,1), ..., x_(0,i-1), t, x_(0, i+1), ..., x_(0,n)) in X} $ is differentiable at $t = x_(0,i)$. Its derivative $g'(x_(0,i))$ at $x_(0,i)$ is denoted $ #form()[$ ((diff f)/(diff x_i))(x_0), space.quad diff_(x_i) f(x_0), space.quad diff_i f(x_0) $] $

  _Intuitively, this definition means that we "freeze" all variables except the $i$-th one, and consider the derivative of the corresponding function of one variable._
]

#lem()[
  Consider $X subset.eq RR^n$ open and $f$, $g$ functions from $X$ to $RR$. Let $1 <= i <= n$.
  + If $f$ and $g$ have partial derivatives with respect to the $i$-th coordinate on $X$, then $f+g$ also does, and $ #form()[$ diff_(x_i)(f+g) = diff_(x_i)(f) + diff_(x_i)(g) $] $
  + If $m=1$, and if $f$ and $g$ have partial derivatives with respect to the $i$-th coordinate on $X$, then
    + $f dot g$ also does and $ #form($ diff_(x_i)(f dot g) = diff_(x_i)(f) dot g + f dot diff_(x_i)(g) $) $
    + if $g(x) != 0$ for all $x in X$, then $f/g$ has a partial derivative with respect to the $i$-th coordinate on $X$, with $ #form($ diff_(x_i)(f/g) = (diff_(x_i)(f) dot g - f dot diff_(x_i)(g))/g^2 $) $
]

=== Jacobi Matrix

#def()[
  Let $X subset.eq RR^n$ open and $f: X -> RR^m$ a function with partial derivatives on X. Write
  $ f(x) = (f_1 (x), ..., f_m (x)) $
  For any $x in X$, the matrix
  $ #form($ J_f(x) = (diff_(x_j) f_i (x))_(1 <= i <= m)^(1 <= j <=n) $) $
  with $m$ rows and $n$ columns is called the *Jacobi matrix* of $f$ at $x$.
]

=== Gradient

#def()[
  Let $X subset.eq RR^n$ be open.
  + Let $f: X -> RR$ be a function. If all partial derivatives of $f$ exists at $x_0 in X$, then the column vector
  $ mat(diff_(x_1) f(x_0); ...; diff_(x_n) f(x_0)) $
  if called the *gradient* of $f$ at $x_0$, and is denoted #form($ Delta f(x_0) $) 
]

== Formula Collection

TODO
