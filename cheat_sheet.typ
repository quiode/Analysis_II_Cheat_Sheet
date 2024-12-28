
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
  columns: 4,
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
#let limit = $attach(lim, tr: x -> x_0, br: x != x_0)$

#underline()[= Analysis II Cheat Sheet]

== Continuity in $RR^n$

Let $x_0 in X subset.eq RR^n$ and $f: X -> RR^m$

=== Convergence

#def()[
  Let $(x_k)_(k in NN)$ where $x_k in RR^n$. Write $ x_k = (x_(k,1), ..., x_(k,n)) $
  Let $y = (y_1, ..., y_n) in RR^n$. We say that the sequence $(x_k)$ *converges* to $y$ as $k -> + infinity$ if $ forall epsilon > 0: exists N >= 1: forall k >= N: ||x_k-y|| < epsilon $
]

#lem()[
  The sequence $(x_k)$ converges to $y$ as $k -> + infinity$ iff#footnote("if and only if")one of the following *equivalent* conditions holds:
  + For each $i$, $i <= i <= n$, the sequence $(x_(k,i))$ of real numbers converges to $y_i$.
  + The sequence of real numbers $||x_k - y||$ converges to $0$ as $k -> + infinity$.
]

=== Continuity

#def()[
  + We say that $f$ is *continuous at $x_0$* if $forall epsilon > 0: exists delta > 0: forall x in X:$ $ ||x - x_0|| < delta ==> || f(x) - f(x_0) || < epsilon $
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
    ||x - x_0|| < delta => || f(x) - y || < epsilon
  $
  We then write $ limit f(x) = y $
]

#lem()[
  We have $ limit f(x) = y $ iff, for every sequence $(x_k)$ in $X$ sucht that $x_k -> x_0$ as $k -> + infinity$, and $x_k != x_0$, the sequence $(f(x_k))$ in $RR^m$ converges to $y$.
]

#lem()[
  Let $Y subset.eq RR^m$ and $p >= 1$ an integer. Let $f: X -> Y$ and $g: Y -> RR^p$ be continuous functions. Then the composite *$g compose f$* is continuous.
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

== Partial Derivatives

Let $X subset.eq RR^n$ be an open set. 

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
  Let $f: X -> RR$ be a function. Let $1 <= i <= n$. We say that $f$ has a *partial derivative* on $X$ with respect to the $i$-th variable, or coordinate, if for all $x_0 = (x_(0,1), ..., x_(0,n) in X)$ the function defined by $ g(t) = f(x_(0,1), ..., x_(0,i-1), t, x_(0, i+1), ..., x_(0,n)) $ on the set $ I = {t in RR: (x_(0,1), ..., x_(0,i-1), t, x_(0, i+1), ..., x_(0,n)) in X} $ is differentiable at $t = x_(0,i)$. Its derivative $g'(x_(0,i))$ at $x_(0,i)$ is denoted $ ((diff f)/(diff x_i))(x_0), space.quad diff_(x_i) f(x_0), space.quad diff_i f(x_0) $

  _Intuitively, this definition means that we "freeze" all variables except the $i$-th one, and consider the derivative of the corresponding function of one variable._
]

#lem()[
  Let $f$, $g$ be functions from $X$ to $RR$. Let $1 <= i <= n$.
  + If $f$ and $g$ have partial derivatives with respect to the $i$-th coordinate on $X$, then $f+g$ also does, and $ diff_(x_i)(f+g) = diff_(x_i)(f) + diff_(x_i)(g) $
  + If $m=1$, and if $f$ and $g$ have partial derivatives with respect to the $i$-th coordinate on $X$, then
    + $f dot g$ also does and $ diff_(x_i)(f dot g) = diff_(x_i)(f) dot g + f dot diff_(x_i)(g) $
    + if $g(x) != 0$ for all $x in X$, then $f/g$ has a partial derivative with respect to the $i$-th coordinate on $X$, with $ diff_(x_i)(f/g) = (diff_(x_i)(f) dot g - f dot diff_(x_i)(g))/g^2 $
]

=== Jacobi Matrix

#def()[
  Let $f: X -> RR^m$ be a function with partial derivatives on $X$. Write
  $ f(x) = (f_1 (x), ..., f_m (x)) $
  For any $x in X$, the matrix
  $ J_f(x) = (diff_(x_j) f_i (x))_(1 <= i <= m)^(1 <= j <=n) $
  with $m$ rows and $n$ columns is called the *Jacobi matrix* of $f$ at $x$.
]

=== Gradient

#def()[
  + Let $f: X -> RR$ be a function. If all partial derivatives of $f$ exists at $x_0 in X$, then the column vector $ mat(diff_(x_1) f(x_0); ...; diff_(x_n) f(x_0)) $ if called the *gradient* of $f$ at $x_0$, and is denoted $Delta f(x_0)$.
  + Let $f = (f_1, ..., f_n): X -> RR^n$ be a function with values in $RR^n$ sucht that all partial derivatives of all coordinates $f_i$ of $f$ exists at $x_0 in X$. Then the real number $ "Tr"(J_f (x_0)) = sum^n_(i=1) diff_(x_i) f_i (x_0), $ the trace of the Jacobi matrix, is called the *divergence* of $f$ at $x_0$, and is denoted $"div"(f)(x_0)$.
]

== The Differential

Let $X subset.eq RR^n$ be an open set. 

#def()[
  Let $f: X -> RR^m$ be a function. Let $u$ be a linear map $RR^n -> RR^m$ and $x_0 in X$. We say that $f$ is *differentiable at $x_0$ with differential u* if
  $ attach(lim, tr: x -> x_0, br: x != x_0) (f(x) - f(x_0) - u(x-x_0)) / (||x - x_0||) = 0 $
  where the limit is in $RR^m$. We then denote $d f (x_0) = u$.

  If $f$ is differentiable at every $x_0 in X$, then we say that $f$ is differentiable on $X$.

  _This definition means that, close to $x_0$ , we can approximate $f(x)$ by the affine#footnote()["An affine function is the composition of a linear function with a translation" - so the vectors do not have to go through $(0,0)$, the origin]-linear function $g: RR^n -> RR^m$ defined by $g(x) = f(x_0) + u(x-x_0)$ with an error that becomes much smaller than $||x-x_0||$ as $x$ gets close to $x_0$._
]

#lem()[
  Let $f: X -> RR^m$ be a function that is differentiable on $X$.
  + The function $f$ is continuous on $X$.
  + The function $f = (f_1, ..., f_m)$ admits all partial derivatives $diff_(x_j) f_i$ for $1 <= i <= m$, $1 <= j <= n$ on $X$.
  + Assume that $m = 1$. Let $x_0 in X$, and let $u(x_1, ..., x_n) = a_1 x_1 + ... + a_n x_n$ be the differential of $f$ at $x_0$. We then have $diff_(x_i) f(x_0) = a_i$ for $1 <= i <= n$.
]

#lem()[
  Let $f: X -> RR^m$ and $g: X -> RR^m$ be differentiable functions on $X$.
  + The function $f+g$ is differentiable with differential $d (f+g) = d f + d g$, and if $m = 1$, then $f dot g$ is differentiable.
  + If $m = 1$ and of $g(x) != 0$ for all $x in X$, then $f / g$ is differentiable.
]

#lem()[
  Let $f : X -> RR^m$ be a function on $X$. If $f$ has all partial derivatives on $X$, and if the partial derivatives of $f$ are continuous on $X$, then $f$ is differentiable on $X$ with differential determined by its partial derivatives, in the sense that the matrix of the differential $d f(x_0)$, with respect to the canonical basis of $RR^n$ and $RR^m$, is the *Jacobi matrix* of $f$ at $x_0$.

  _So in simpler terms: The Jacobi matrix is the differential._
]

#lem()[
  Let $Y subset.eq RR^m$ be open and let $f: X -> Y$ and $g: Y -> RR^p$ be differentiable functions. Then $g compose f: X -> RR^p$ is differentiable on $X$, and for any $x_0 in X$, its differential is given by the composition
  $ d (g compose f)(x_0) = d g (f(x_0)) compose d f(x_0) $
  In particular, the Jacobi matrix satisfies
  $ J_(g compose f) (x_0) = J_g (f(x_0)) J_f (x_0) $
  where the right-hand side is a matrix product.
]

=== Tangent Space

#def()[
  Let $f: X -> RR^m$ be a function that is differentiable. Let $x_0 in X$ and $u = d f(x_0)$ be a differential of $f$ at $x_0$. The graph of the affine linear approximation
  $ g(x) = f(x_0) + u(x - x_0) $
  from $RR^n$ to $RR^m$, or in other words the set
  $ {(x,y) in RR^n times RR^m: y = f(x_0) + u(x-x_0)} $
  is called the *tangent space* at $x_0$ to the graph of $f$.
]

=== Directional Derivative

#def()[
  Let $f: X -> RR^m$ be a function. Let $v in RR^n$ be a non-zero vector and $x_0 in X$. We say that $f$ has *directional derivative* $w in RR^m$ in the direction $v$, if the function $g: I -> RR^m$ defined on the set $I = {t in RR: x_0 + t v in X}$ by $g(t) + f(x_0 + t v)$ has a derivative at $t = 0$, and this is equal to $w$. We write this as $D_v f = w$.
]

#lem()[
  Let $f: X -> RR^m$ be a differentiable function. Then for any $x in X$ and non-zero $v in RR^n$, the function $f$ has a directional derivative at $x_0$ in the direction $v$, equal to $d f(x_0)(v)$.
  \
  \
  _What is important to notice in this proposition, is that the
values of the directional derivatives are linear with respect to the vector $v$._
]

== Higher Derivatives

Let $X subset.eq RR^n$ be an open set. 

#def()[
  Let $f: X -> RR^m$.
  We say that $f$ is of class $C^1$ if $f$ is differentiable on $X$ and all its partial derivatives are continuous. The set of functions of class $C^1$ from $X$ to $RR^m$ is denoted $C^1 (X;RR^m)$. 
  
  Let $k >= 2$. We say, by induction, that $f$ is of class $C^k$ if it is differentiable and each partial derivative $diff_(x_i) f : X -> RR^m$ is of class $C^(k-1)$. The set of functions of class *$C^k$* from $X$ to $RR^m$ is denoted $C^k (X; RR^m)$.

  If $f in C^k (X; RR^m)$ for all $k >= 1$, then we say that $f$ is of class $C^infinity$. The set of sucht functions is denoted $C^infinity (X; RR^m)$.
]

#lem()[
  Let $K >= 2$ and $f: X -> RR^m$ be a function of class $C^k$. Then the partial derivatives of order $k$ are independent of the order in which the partial derivatives are taken: for any variables x and y, we have:
  $ diff_(x,y) f = diff_(y,x) f, $
  and for any variables $x$, $y$, $z$, we have
  $ diff_(x,y,z) f = diff_(x,z,y) f = diff_(y,z,x) f = diff_(z,x,y) f = ... $
  etc...
]

=== Hessian Matrix

#def()[
  Let $f: X -> RR$ a $C^2$ function. For $x in X$, the *Hessian matrix* of $f$ at $x$ is the *symmetric square* matrix
  $ "Hess"_f (x) = (diff_(x_i , x_j) f(x))_(1 <= i,j <= n) $
  We also sometimes write simply $H_f (x)$.
]

== Taylor Polynomials

== Formula Collection

TODO
