#import "@preview/fletcher:0.5.3" as fletcher: diagram, node, edge, shapes

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

#set enum(numbering: "1a1.")
#set underline(offset: 1pt)

// VARIABLES

#let _block = block.with(inset: 4pt, radius: 2pt, width: 100%, breakable: true);
#let def(body) = _block(body, stroke: blue)
#let lem(body) = _block(body, stroke: green)
#let note(body) = _block(body, stroke: orange)
#let form(body) = _block(body, stroke: black)
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
  + Let $f: X -> RR$ be a function. If all partial derivatives of $f$ exists at $x_0 in X$, then the column vector $ mat(diff_(x_1) f(x_0); ...; diff_(x_n) f(x_0)) $ if called the *gradient* of $f$ at $x_0$, and is denoted $gradient f(x_0)$.
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

Let $k >= 1$ be an integer. Let $f: X -> RR$ be a function of class $C^k$ on $X$, and fix $x_0 in X$.

#def()[
  The $k$-th *Taylor polynomial* of $f$ at the point $x_0$ is the polynomial in $n$ variables of degree $<=k$ given by
  $
    T_k f(y;x_0) = f(x_0) + sum^n_(i=1) (diff f) / (diff x_i) (x_0) y_i + ... \ + sum_(m_1 + ... + m_n = k) 1 / (m_1 ! dots.c m_n !) (diff^k f) / (diff x_1^(m_1) dots.c diff x_n^(m_n)) (x_0) y_1^(m_1) dots.c y_n^(m_n)
  $
  where the last sum ranges over the tuples of $n$ non-negative integers such that the sum is $k$.

  ==== *$k = 1$*
  $ T_1 f(y; x_0) = f(x_0) + sum^n_(i=1) diff_(x_i) f(x_0) y_i $

  ==== *$k = 2$*
  $
    T_2 f(y; x_0) &= f(x_0) &&+ sum^n_(i=1) diff_(x_i) f(x_0) y_i \ & &&+ 1 / 2 sum^n_(i=1) diff^2_(x^2_i) f(x_0) y_i^2 \ & &&+ sum_(1 <= i < j <= n) diff^2_(x_i x_j) f(x_0) y_i y_j \
    &= f(x_0) &&+ gradient f(x_0) dot y + 1 / 2 y^t "Hess"_f (x_0) y
  $
  _Note that this can also be used to easily *calculate the Hesse Matrix* from a known Taylor polynomial._
]

#lem()[
  If we define $E_k f(x; x_0)$ by $ f(x) = T_k f(x - x_0; x_0) + E_k f(x; x_0) $ then we have $ lim^(x -> x_0)_(x != x_0) (E_k f(x; x_0))/(||x-x_0||^k) = 0. $
]

#note()[
  When we want to find the Taylor Polynomial of $n$-th order of a function that consists of other functions with already known (Taylor) Polynomials, we can just replace each function with their respective Taylor Polynomial and "ignore" all terms with order higher than $n$.
]

== Critical Points

Let $X subset.eq RR^n$ be open and $f: X -> RR$ a differentiable function.

#lem()[
  If $x_0 in X$ is such that
  - $f(y) <= f(x_0)$ for all $y$ close enough to $x_0$ (local maximum at $x_0$) _or_
  - $f(y) >= f(x_0)$ for all $y$ close enough to $x_0$ (local minimum of $x_0$).
  Then we have $d f(x_0) = 0$, or in other words $gradient f(x_0) = 0$, or equivalently $(diff f) / (diff x_i) (x_0) = 0$ for $1 <= i <= n$.
]

#def()[
  A point $x_0 in X$ such that $gradient f (x_0)$ is called a *critical point* of the function $f$.
]

#def()[
  Let $f in C^2$. A critical point $x_0 in X$ of $f$ is called *non-degenerate* if the Hessian matrix has non-zero determinant.
]

#lem()[
  Let $f in C^2$. Let $x_0$ be a non-degenerate critical point of $f$. Let $p$ and $q$ be the number of positive and negative eigenvalues#footnote()[can be derived using the formulas in the eigenvalues section] of $"Hess"_f (x_0)$.
  + If $p = n$, equivalently if $q = 0$, the function $f$ has a *local minimum* at $x_0$.
  + If $q = n$, equivalently if $p = 0$, the function $f$ has a *local maximum* at $x_0$.
  + Otherwise, equivalent if $p q != 0$, the function $f$ does not have a local extremum at $x_0$. One then says that $f$ has a *saddle point* at $x_0$.
  \
  \
  The condition $ p = n$ means that the Hessian matrix $H$ at $x_0$ is a *positive definite symmetric matrix* (and $q = n$ means that it is a negative definite matrix). This also means that $y^t H y > 0$ for any non-zero vector $y in RR^n$. When $p q != 0$, the Hessian is also said to be *indefinite*.

  === *$n=2$*

  $ mat(a,b;b,d) "positive definite" <=> a > 0, a d - b^2 > 0 $
  $ mat(a,b;b,d) "negative definite" <=> a < 0, a d - b^2 > 0 $
  $ mat(a,b;b,d) "indefinite" <=> a d - b^2 < 0 $

  #diagram(
    node-stroke: black + 0.5pt,
    spacing: (0.25cm, 1cm),
    edge((1, -0.75), auto, "-|>"),
    node((1, 0), $det (A)$, shape: shapes.diamond),
    node((0, 1), $tr (A)$, shape: shapes.diamond),
    node((2, 1), $tr (A)$, shape: shapes.diamond),
    node((0, 2), "indef."),
    node((1, 2), "neg. semidef."),
    node((2, 2), $A = mat(0,0;0,0)$),
    node((0, 3), "pos. semidef."),
    node((1, 3), "neg. def."),
    node((2, 3), "pos. def."),
    edge((1, 0), (0, 2), "-|>", `neg.`, bend: +10deg, label-pos: 0.1),
    edge((1, 0), (0, 1), "-|>", $0$),
    edge((1, 0), (2, 1), "-|>", `pos.`),
    edge((0, 1), (1, 2), "-|>", `neg.`, label-pos: 0.75),
    edge((0, 1), (2, 2), "-|>", $0$, bend: +30deg),
    edge((0, 1), (0, 3), "-|>", `pos.`, bend: +33deg, label-pos: 0.75),
    edge((2, 1), (1, 3), "-|>", `neg.`, bend: +30deg, label-pos: 0.7),
    edge((2, 1), (2, 3), "-|>", `pos.`, bend: +20deg, label-pos: 0.75),
  )

  === *$n = 3$*
  $ mat(a,b,c;b,e,f;c,f,i) "positive definite" <=> \ a > 0, space.quad a e - b^2 > 0, space.quad det (A) > 0 $
]

== Lagrange Multipliers

Let $X subset.eq RR^n$ be open and $f: X -> RR$ a differentiable function.

#lem()[
  Let $g: X -> RR, f in C^1$. If $x_0 in X$ is a local extremum of the function $f$ restricted to the set $ Y = {x in X: g(x) = 0} $ then either $gradient g(x_0) = 0$, or there exists $lambda in RR$ such that $ cases(gradient f(x_0) = lambda gradient g (x_0), g(x_0) = 0\,) $ or in other words, there exists $lambda$ such that $(x_0, lambda)$ is a critical point of the differentiable function $h: X times RR -> RR$ defined by $ h(x, lambda) = f(x) - lambda g(x). $ Such a value $lambda$ is called a *Lagrange multiplier* at $x_0$.
]

== Inverse Theorem

Let $X subset.eq RR^n$ be open and $f: X -> RR^n$ be differentiable.

#def()[
  Let $x_0 in X$. We say that $f$ is a *change of variable* around $x_0$ if there is a radius $r > 0$ sucht that the restriction of $f$ to the ball $ B = {x in RR^n: ||x - x_0|| < r} $ of radius $r$ around $x_0$ has the property that the image $Y = f(B)$ is open in $RR^n$, and if there is a differentiable map $g: Y -> B$ such that $f compose g = "Id"_Y$ and $g compose f = "Id"_B$.
]

#def()[
  If $x_0 in X$ is such that $det (J_f (x_0)) != 0$, i.e., such that the Jacobian matrix of $f$ at $x_0$ is invertible, then $f$ is a change of variable around $x_0$. Moreover, the Jacobian of $g$ at $x_0$ is determined by $ J_g (f(x_0)) = J_f (x_0)^(-1). $ In addition, if $f$ is of class $C^k$, then $g$ is of class $C^k$.
]

== Implicit Function Theorem

#def()[
  Let $X subset.eq RR^(n+1)$ be open and let $g: X -> RR$ be of class $C^k$ with $k >= 1$. Let $(x_0, y_0) in RR^n times RR$ be such that $g(x_0, y_0) = 0$. Assume that $ diff_y g(x_0, y_0) != 0. $ Then there exists an open set $U subset.eq RR^n$ containing $x_0$, an open interval $I subset.eq RR$ containing $y_0$, and a function $f: U -> RR$ of class $C^k$ such that the system of equations $ cases(g(x, y) = 0, x in U\, space y in I) $ is equivalent with $y = f(x)$. In particular, $f(x_0) = y_0$. Moreover, the gradient of $f$ at $x_0$ is given by $ gradient f(x_0) = - 1/((diff_y g)(x_0, y_0)) gradient_x g(x_0, y_0), $ where $gradient_x g = (diff_(x_1) g, ..., diff_(x_n) g)$.
]

== Ordinary Differential Equations

_A differential equation is an equation where the unknown (or unknowns) is a function $f$, and the equation relates values of $f$ at a point $x$ with values of derivatives of the function at the same point $x$. If the function has one variable only, one speaks of ordinary differential equations._

#def()[
  Suppose $F: RR^2 -> RR$ is a continuously differentiable function of two variables. Let $x_0 in RR$ and $y_0 in RR$. Then the *ordinary differential equation* $ y' = F(x,y) $ has a unique solution $f$ defined on a "largest" open interval $I$ containing $x_0$ such that $f(x_0) = y_0$. In other words, there exists $I$ and a function $f: I -> RR$ such that for all $x in I$, we have $f'(x) = F(x, f(x))$, and one cannot find a larger interval containing $I$ with such a solution.
]

== Linear Differential Equations

#def()[
  Let $I subset.eq RR$ be an open interval and $k >= 1$ an integer. An *homogeneous linear ordinary differential equation* of order $k$ on $I$ is an equation of the form $ y^((k)) + a_(k-1) y^((k-1)) + dots.c + a_1 y' + a_0 y = 0 $ where the coefficients $a_0, ..., a_(k-1)$ are complex-valued functions on $I$, and the unknown is a complex valued function from $I$ to $CC$ that is $k$-times differentiable on $I$.

  An equation of the form $ y^((k)) + a_(k-1) y^((k-1)) + dots.c + a_1 y' + a_0 y = b $ where $b: I -> CC$ is another function, is called an *inhomogeneous linear ordinary differential equation*, with associated homogeneous equation the one with $b = 0$.
]

#def()[
  Let $I subset.eq RR$ be an open interval and $k >= 1$ an integer, and let $y^((k)) + a_(k-1) y^((k-1)) + dots.c + a_1 y' + a_0 y = 0$ be a linear differential equation over $I$ with continuous coefficients.
  + The set $S$ of $k$-times differentiable solutions $f: I -> CC$ of the equation is a complex vector space which is a subspace of the space of complex-valued functions on $I$.
    + If the functions $a_i$ are real-valued, the set $S$ of real-valued solutions is a real vector space which is a subspace of the space of real-valued functions on $I$.
  + The dimension of $S$ is $k$, and for an choice of $x_0 in I$ and any $(y_0, ..., y_(k-1)) in CC^k$, there exists a unique $f in S$ such that $ f(x_0) = y_0, space f'(x_0) = y_1, space ..., space f^((k-1)) (x_0) = y_(k-1). $
    + If the functions $a_i$ are real-valued, the dimension of the space of real-valued solutions, as a real vector space, is $k$, and for any choice of $x_0 in I$ and any $(y_0, ... y_(k-1)) in RR^k$, there exists a unique real-valued solution $f$ such that $ f(x_0) = y_0, space f'(x_0) = y_1, space ..., space f^((k-1)) (x_0) = y_(k-1). $ If $b$ and the coefficients of $a_i$ are real-valued, there exists a real-valued solution.
  + Let $b$ be a continuous function on $I$. There exists a solution $f_0$ to the inhomogeneous equation $ y^((k)) + a_(k-1) y^((k-1)) + dots.c + a_1 y' + a_0 y = b, $ and the set $S_b$ is the set of functions $f + f_0$ where $f in S$.
  + For any $x_0 in I$ and any $(y_0, ..., y_(k-1)) in CC^k$, there exists a unique $f in S_b$ such that $ f(x_0) = y_0, space f'(x_0) = y_1, space ..., space f^((k-1)) (x_0) = y_(k-1). $
]

== (In)homogene ODE's

#lem()[
  #set text(6pt)

  We focus on $ y^((k)) + a_(k-1) y^((k-1)) + ... + a_0 y = b $ with continuous $a_0, ..., a_(k-1), b$.

  #show table.cell: it => {
    if it.y == 0 or calc.odd(it.y) {
      strong(it)
    } else {
      it
    }
  }

  #align(
    center,
    table(
      columns: (1fr, 1fr),
      stroke: (x, y) => {
        if x == 0 and calc.even(y) {
          (right: 0.7pt + black)
        }
        (top: 0.7pt + black, bottom: 0.7pt + black)
      },
      table.header(
        underline()[Solutions],
        underline()[real-valued Solutions#footnote([if $a_0,...,a_(n-1), b$ real-valued])],
        table.hline(),
      ),
      table.cell(colspan: 2)[Homogeneous ODE], table.hline(),
      [create a vector space $S$ with $dim_CC (S) = k$], [create a $RR$-vector space $S_RR$ of $S$ with $dim_RR (S_RR) = k$],
      table.cell(colspan: 2)[Inhomogeneous ODE], table.hline(),
      [has a solution for $f_0$ with solution space $= f_0 + S = {f_0 + f | f in S}$], [has a real-valued solution $f_0$ with real-valued solution space $= f_0 + S_RR$],
      table.cell(
        colspan: 2,
      )[Initial Value Problem (IVP/AWP) \ $y(x_0) = y_0$ \ $y'(x_0) = y_1,...,y^((k-1)) (x_0) = y_(k-1)$], table.hline(),
      [has exactly one solution], [has exactly one real-valued solution],
    ),
  )
]

== (Solving) Linear Differential Equations Of Order 1

#text(red)[TODO!] - use the lectures, not the script

#note()[
  Let $I subset.eq RR$ be an open interval. We consider the linear differential equation $ y' + a y = b, $ when $a$ and $b$ are general continuous functions defined on $I$

  The solution has two steps:
  + Solving the homogeneous equation $y' + a y = 0$.
  + Finding a solution $f_'$ of the inhomogeneous equation, so that the set $S_b$ contains exactly the functions $f_0 + f$ where $f in S$.

  === Step 1 (solving the homogeneous equation)
  #lem()[
    Any solution of $y' + a y = 0$ is of the form $f(x) = z exp (- A(x))$ where $A$ is a primitive of $a$. The unique solution with $f(x_0) = y_0$ is $ f(x) = y_0 exp (A(x_0) - A(x)). $
  ]

  === Step 2 (solving the inhomogeneous equation)

]

== Linear Differential Equations With Constant Coefficients

#text(red)[TODO!]

== Line Integrals

#def()[
  + Let $I = [a, b]$ be a closed and bounded interval in $RR$. Let $f(t) = (f_1 (t), ..., f_n (t))$ be a continuous function from $I$ to $RR^n$, i.e., $f_i$ is continuous for $1 <= i <= n$. Then we define $ integral^b_a f(t) d t = (integral^b_a f_1 (t) d t, ..., integral^b_a f_n (t) d t) in RR^n. $
  + A *parameterized curve* in $RR^n$ is a continuous map $gamma : [a, b] -> RR^n$ that is piecewise $C^1$, i.e., there exists $k >= 1$ and a partition $ a = t_0 < t_1 < dots.c < t_(k-1) < t_k = b $ such that the restriction of $gamma$ to $] t_(j-1), t_j [$ is $C^1$ for $1 <= j <= k$. We say that $gamma$ is a parameterized curve, or a *path*, between $gamma (a)$ and $gamma (b)$.
  + Let $gamma: [a, b] -> RR^n$ be a parameterized curve. Let $X subset.eq RR^n$ be a subset containing the image of $gamma$, and let $f: X -> RR^n$ be a continuous function. The integral $ integral^b_a f(gamma (t)) dot gamma'(t) d t in RR $ is called the *line integral of $f$ along $gamma$*. It is denoted $ integral_gamma f(s) dot d s space.quad "or" space.quad integral_gamma f(s) dot d arrow(s). $
]

#lem()[
  The integral of continuous functions $I -> RR^n$ satisfy much of the same rules as the Riemann integral of a function $I -> RR$, for instance $ integral^b_a (f(t) + g(t)) d t = integral^b_a f(t) d t + integral^b_a g(t) d t. $ Also, as in the one-variable case, we define $ integral^a_b f(t) d t = - integral^b_a f(t) d t, $ if $a < b$.

  In the line integral, $gamma'(t)$ and $f(gamma(t))$ are both vectors in $RR^n$ for all $t$, so that the final integral is a real number.

  It is customary, when working with line integrals, to say that the function $f: X -> RR^n$ is a *vector field*: a function that sends each point $x$ in $X subset.eq RR^n$ to a vector in $RR^n$, which we display as based at $x$.
]

=== Reparameterization

#def()[
  Let $gamma : [a, b] -> RR^n$ be a parameterized curve. An *oriented reparameterization* of $gamma$ is a parameterized curve $sigma : [c,d] -> RR^n$ such that $sigma = gamma compose phi$, where $phi : [c, d] -> [a, b]$ is a continuous map, differentiable on $]c,d[$, that is strictly increasing and satisfies $phi (c) = a$ and $phi (d) = b$.
]

#lem()[
  Let $gamma$ be a parameterized curve in $RR^n$ and $sigma$ an oriented reparameterization of $gamma$. Let $X$ be a set containing the image of $gamma$, or equivalently the image of $sigma$, and $f: X -> RR^n$ a continuous function. Then we have $ integral_gamma f(s) dot d arrow(s) = integral_sigma f(s) dot d arrow(s). $
]

=== Conservativeness

#def()[
  Let $X subset.eq RR^n$ and $f: X -> RR^n$ be a continuous vector field. If, for any $x_1$, $x_2$ in $X$, the line integral $ integral_gamma f(s) dot d arrow(s) $ is independent of the choice of a parameterized curve $gamma$ in $X$ from $x_1$ to $x_2$, then we say that the vector field is *conservative*.
]

#lem()[
  Equivalently, $f$ is conservative iff $ integral_gamma f(s) dot d arrow(s) = 0 $ for any closed#footnote([$gamma (a) = gamma(b)$]) parameterized curve in $X$.
]

#lem()[
  Let $X$ be an open set and $f$ a conservative vector field. Then there exists a $C^1$ function $g$ on $X$ such that $f = gradient g$.

  If any two points of $X$ can be joined by a parameterized curve#footnote([so if $X$ is path-connected, so when $X$ is convex]), then $g$ is unique up to addition of a constant: if $gradient g_1 = f$, then $g - g_1$ is constant on $X$.

  If $f$ is a conservative vector field on $X$, then a function $g$ such that $gradient g = f$ is called a *potential* for $f$.
]

#lem()[
  Let $X subset.eq RR^n$ be an open set and $f: X -> RR^n$ a vector field of class $C^1$. Write $f(x)= (f_1(x), ..., f_n(x)).$ If $f$ is conservative, then we have $ (diff f_i) / (diff x_j) = (diff f_j) / (diff x_i) $ for any integers with $1 <= i != j <= n$.
]

#def()[A subset $X subset.eq RR^n$ is *star shaped* if there exists $x_0 in X$ such that, for all $x in X$, the line segment joining $x_0$ to $x$ is contained in $X$. We then also say that $X$ is star-shaped around $x_0$.]

#lem()[
  Let $X$ be a star-shaped open subset of $RR^n$. Let $f$ be a $C^1$ vector field such that $ (diff f_i) / (diff x_j) = (diff f_j) / (diff x_i) $ on $X$ for all $i != j$ between $1$ and $n$. Then the vector field $f$ is conservative.
]

=== Curl

#def()[
  Let $X subset.eq RR^3$ be an open set and $f: X -> RR^3$ a $C^1$ vector field. Then the curl of $f$, denoted $"curl"(f)$, is the continuous vector field on $X$ defined by $ "curl"(f) = vec(diff_y f_3 - diff_z f_2, diff_z f_1 - diff_x f_3, diff_x f_2 - diff_y f_1), $ where $f(x,y,z) = (f_1 (x,y,z), f_2 (x,y,z), f_3 (x,y,z))$.
]

#lem()[
  For a $3$-dimensional vector field, $"curl"(f) = 0$ means precisely that $f$ is conservative.
]

== The Riemann Integral in $RR^n$

#def()[
  For any bounded closed subset $X subset.eq RR^n$ and any continuous function $f: X -> RR$, one can define the *integral of $f$ over $X$*, denoted $ integral_X f(x) d x, $ which is a real number, depending of course on $X$ and on $f$.
]

#lem()[
  === Compatibility

  If $n = 1$ and $X = [a, b]$ is an interval, then the integral of $f$ over $X$ is the Riemann integral of $f$: $ integral_([a, b]) f(x) d x = integral^b_a f(x) d x. $

  === Linearity

  If $f$ and $g$ are continuous on $X$ and $a$, $b$ are real numbers, then $ integral_X (a f_1 (x) + b f_2 (x)) d x = \ a integral_X f_1 (x) d x + b integral_X f_2 (x) d x. $

  === Positivity

  If $f <= g$, then $ integral_X f(x) d x <= integral_X g(x) d x $ and especially, if $f >= 0$, then $ integral_X f(x) d x >= 0. $ Moreover, if $Y subset.eq X$ is compact and $f >= 0$, then $ integral_Y f(x) d x <= integral_X f(x) d x. $

  === Upper Bound And Triangle Inequality

  In particular, since $-|f| <= f <= |f|$, we have $ |integral_X f(x) d x| <= integral_X |f(x)| d x, $ and since $|f+g| <= |f| + |g|$, we have $ |integral_X (f(x) + g(x)) d x| <= integral_X |f(x)| d x + integral_X|g(x)| d x. $

  === Volume

  If $f = 1$, then the integral of $f$ is the *volume* in $RR^n$ of the set $X$, and if $f >= 0$ in general, the integral of $f$ is the volume of the set $ {(x,y) in X times RR : 0 <= y <= f(x)} subset.eq RR^(n+1). $ In particular, if $X$ is a bounded rectangle, say $ X = [a_1, b_1] times dots.c times [a_n, b_n] subset.eq RR^n $ and $f = 1$, then $ integral_X d x = (b_n - a_n) dots.c (b_1 - a_1). $ We write $"Vol"(X)$ or $"Vol"_(n)(X)$ for the volume of $X$.

  === Multiple Integral / Fubini's Theorem

  If $n_1$ and $n_2$ are integers $>= 1$ such that $n = n_1 + n_2$, then for $x_1 in RR^(n_1)$, let $ Y_(x_1) = {x_2 in RR^(n_2) : (x_1, x_2) in X} subset.eq RR^(n_2). $ Let $X_1$ be the set of $x_1 in RR^n$ such that $Y_(x_1)$ is not empty. Then $X_1$ is compact in $RR^(n_1)$ and $Y_(x_1)$ is compact in $RR^(n_2)$ for all $x_1 in X_1$. If the function $ g(x_1) = integral_(Y_(x_1)) f(x_1, x_2) d x_2 $ on $X_1$ is continuous, then $ integral_X f(x_1, x_2) d x = integral_(X_1) g(x_1) d x_1 = \ integral_(X_1) (integral_(Y_(x_1)) f(x_1, x_2) d x_2) d x_1. $ Similarly, exchanging the role of $x_1$ and $x_2$, we have $ integral_X f(x_1, x_2) d x = integral_(X_2) (integral_(Z_(x_2)) f(x_1, x_2) d x_1) d x_2, $ where $Z_(x_2) = {x_1 : (x_1, x_2) in X}$, if the integral over $x_1$ is a continuous function.

  ==== Special Cases

  / $X = X_1 times X_2$: Assuming $X_1 subset.eq RR^(n_1)$, $X_2 subset.eq RR^(n_2)$, $f$ is continuous and $Y_(x_1) = {x_2 in RR^(n_2): (x_1, x_2) in X_1 times X_2} = X_2 subset.eq RR^(n_2)$, then $ integral_(X_1 times X_2) f(x_1, x_2) d x_1 d x_2 = \ integral_(X_1) (integral_(X_2) f(x_1, x_2) d x_2) d x_1 = \ integral_(X_2) (integral_(X_1) f(x_1, x_2) d x_1) d x_2. $
  / $X = [a_1, b_1] times dots.c times [a_n, b_n] subset.eq RR^n$: Assuming $f(x_1, ..., x_n) = f_1 (x_1) dots.c f_n (x_n)$ and each $f_i$ is continuous, then $ integral_X f(x_1, ..., x_n) d x_1 dots.c d x_n = \ (integral^(b_1)_(a_1) f_1 (x) d x) dots.c (integral^(b_n)_(a_n) f_n (x) d x) $

  === Domain Additivity

  If $X_1$ and $X_2$ are compact subsets of $RR^n$, and $f$ is continuous on $X_1 union X_2$, then $ integral_(X_1 union X_2) f(x) d x + integral_(X_1 sect X_2) f(x) d x = \ integral_(X_1) f(x) d x + integral_(X_2) f(x) d x. $
  If $X_1 sect X_2$ is *empty*, then $X_1 union X_2$, then $ integral_(X_1 union X_2) f(x) d x = integral_(X_1) f(x) d x + integral_(X_2) f(x) d x. $
]

=== Negligibleness

#def()[
  + Let $1 <= m <= b$ be an integer. A *parameterized $m$-set* in $RR^n$ is a continuous map $ f: [a_1, b_1] times dots.c times [a_m, b_m] -> RR^n $ which is $C^1$ on $ ]a_1, b_1[ times dots.c times ]a_b, b_m [ . $
  + A subset $B subset.eq RR^n$ is *negligible* if there exist an integer $k >= 0$ and parameterized $m_i$-sets $f_i : X_i -> RR^n$, with $1 <= i <= k$ and $m_i < n$, such that $ B subset.eq f_1 (X_1) union dots.c union f_k (X_k). $
]

#lem()[
  - Any subset of the real axis $RR times {0} subset.eq RR^2$ is negligible in $RR^2$.
  - If $H subset.eq RR^n$ is an affine subspace of dimension $m < n$, then any subset of $RR^n$ that is contained in $H$ is negligible.
  - The image of a parameterized curve $gamma : [a,b] -> RR^n$ is negligible, since $gamma$ is a $1$-set in $RR^n$.
]

#lem()[
  Let $X subset.eq RR^n$ be a compact set. Assume that $X$ is negligible. Then for any continuous function on $X$, we have $ integral_X f(x) d x = 0. $
]

== Improper Integrals

#def()[
  Let $f$ be continuous on $RR^2$. Assume that $f >= 0$. We say that $f$ is Riemann-integrable on $RR^2$, if the limit $ lim_(R -> + infinity) integral_([-R, R]^2) f(x, y) d x d y $ exists, which is then called the *integral of $f$ over $RR^2$* and denoted $ integral_(RR^2) f(x,y) d x d y. $

  One can then show that this integral is also the limit of $ integral_(D_R) f(x,y) d x d y$ where $D_R$ is the disc of radius $R$ centered at $0$.

  Also $ integral_(RR^2) f(x,y) d x d y = \ integral^(+ infinity)_(- infinity) (integral^(+ infinity)_(- infinity) f(x, y) d y) d x = \ integral^(+ infinity)_(- infinity) (integral^(+ infinity)_(- infinity) f(x, y) d x) d y. $
]

== The Change Of Variable Formula

#def()[
  Let $dash(X) subset.eq RR^n$ and $dash(Y) subset.eq RR^n$ be compact subsets. Let $phi : dash(X) -> dash(Y)$ be a continuous map. We assume that we can write $dash(X) = X union B$ and $dash(Y) = Y union C$ where
  + the sets $X$ and $Y$ are open;
  + the sets $B$ and $C$ are negligible;
  + the restriction of $phi$ to the open set $X$ is a $C^1$ bijective map from $X$ to $Y$.

  In this situation, the Jacobian matrix $J_phi (x)$ is invertible at all $x in X$; we assume that we can find a continuous function on $dash(X)$ that restricts to $det (J_phi (x))$ on $X$. We abuse notation and still write $det (J_phi (x))$ for this function, even if $x in B$.
  \
  \
  In the situation described above, for any continuous function $f$ on $dash(Y)$, we have $ integral_(dash(X)) f(phi (x)) | det (J_phi (x)) | d x = integral_(dash(Y)) f(y) d y. $
]

#lem()[
  *Polar coordinates* $(r, theta)$ are useful for integrating over a disc in $RR^2$ centered at $0$, or more generally over a disc sector $Delta = Delta (a,b,R)$ defined by $ 0 <= r <= R, space.quad -pi < a <= theta <= b < pi $ for some parameters $(a,b,R)$. One then gets the formula $ integral_Delta f(x,y) d x d y = integral^R_0 integral^b_a f(r cos (theta), r sin (theta)) r d r d theta . $ Taking $r$ to vary between $0 < r_0 <= r <= R$, we obtain an annulus#footnote([region between two concentric circles]).
]

#lem()[
  *Spherical coordinates* $(r, theta, phi)$ in $RR^3$ are useful for integrating over balls centered at $0$, or parts of them. For integrating a function $f$ over a ball $B$ of radius $R$ in $RR^3$, we have the formula $ integral_B f(x,y,z) d x d y d z = integral^R_b integral^(2 pi)_0 integral^(pi)_0 \ f(r cos (theta) sin (phi), r sin (theta) sin (phi), r cos(phi)) r^2 sin (phi) \ d r d theta d phi $
]

== Green's Formula

#def()[
  A simple closed parameterized curve $gamma : [a, b] -> RR^2$ is a closed parameterized curve such that $gamma (t) != gamma (s)$ unless $t = s$ or ${s,t} = {a,b}$, and such that $gamma ' (t) != 0$ for $a < t < b$. (If $gamma$ is only piecewise $C^1$ inside $]a,b[$, this condition only applies where $gamma ' (t)$ exists).
]

#lem()[
  Let $X subset.eq RR^2$ be a compact set with a boundary $diff X$ that is the union of finitely many simple closed parameterized curves $gamma_1, dots, gamma_k$. Assume that $ gamma_i : [a_i, b_i] -> RR^2 $ has the property that $X$ lies always "to the left" of the tangent vector $gamma_i ' (t)$ based at $gamma_i (t)$. Let $f = (f_1, f_2)$ be a vector field of class $C^1$ defined on some open set containing $X$. Then we have $ integral_X ((diff f_2) / (diff_x) - (diff f_1) / (diff y)) d x d y = sum^k_(i = 1) integral_(gamma_1) f dot d arrow(s). $

  _Note that if this condition is not met, it simply means that one must "reverse" the corresponding curve, e.g., replace $gamma : [0,1] -> RR^2$ by $tilde(gamma) (t) = gamma (1-t)$ for $0 <= t <= 1$, which reverses the orientation of the tangent vector._
]

#lem()[
  Let $X subset.eq RR^2$ be a compact set with a boundary $diff X$ that is the union of finitely many simple closed parameterized curves $gamma_1, ..., gamma_k$. Assume that $ gamma_i = (gamma_(i,1), gamma_(i,2)) : [a_i, b_i] -> RR^2 $ has the property that $X$ lies always "to the left" of the tangent vector $gamma_i ' (t)$ based at $gamma_i (t)$. Then we have $ "Vol"(X) = sum^k_(i = 1) integral_(gamma_i) x dot d arrow(s) = sum^k_(i = 1) integral^(b_i)_(a_i) gamma_(i,1) (t) gamma_(i,2) ' (t) d t . $
]

== The Gauss-Ostrogradski formula

#def()[
  A parameterized surface $sum : [a,b] times [c,d] -> RR^3$ is a $2$-set in $RR^3$ such that the rank of the Jacobian matrix is $2$ at all $(s,t) in ]a,b[ times ]c,d[$.
]

#def()[
  Let $x$ and $y$ be two linearly independent vectors in $RR^3$. The *vector product*, or *cross product* $z = x times y$ is the unique vector in $RR^3$ such that $(x,y,z)$ is a basis of $RR^3$ with $det (x,y,z) > 0$, and $ ||z|| = ||x||||y|| sin (theta), $ where $theta$ is the angle between $x$ and $y$.
]

#lem()[
  Let $X subset.eq RR^3$ be a compact set with a boundary $diff X$ that is parameterized surface $sum : [a,b] times [c,d] -> RR^3$. Assume that $sum$ is injective in $]a,b[ times ]c,d[$, and that $sum$ has the property that the normal vector $arrow(n)$ points away from $sum$ at all points. Let $arrow(u) = arrow(n)/(||arrow(n)||)$ be the unit exterior normal vector. Let $f = (f_1, f_2, f_3)$ be a vector field of class $C^1$ defined on some open set containing $X$. Then we have $ integral_X "div"(f) d x d y d z = integral_sum (f dot arrow(u)) d sigma. $
]

== Formula Collection

#show math.equation: set block(breakable: true) // equations in the collection should wrap pages

=== Functions

#figure(image("Functions.svg", width: 100%))

=== Trigonometric Functions

#form()[
  $ + pi <=> dot -1 $

  #table(
    columns: (1fr, 1fr, 1fr, 1fr, 1fr),
    table.header([*deg*], [*rad*], [*sin*], [*cos*], [*tan*]),
    stroke: (x, y) => if y == 0 {
      (bottom: 0.7pt + black)
    },
    $0 degree$, $0$, $0$, $1$, $0$,
    $30 degree$, $pi / 6$, $1 / 2$, $sqrt(3) / 2$, $sqrt(3) / 3$,
    $45 degree$, $pi / 4$, $sqrt(2) / 2$, $sqrt(2) / 2$, $1$,
    $60 degree$, $pi / 3$, $sqrt(3) / 2$, $1 / 2$, $sqrt(3)$,
    $90 degree$, $pi / 2$, $1$, $0$, $"N/A"$,
    $120 degree$, $(2 pi) / 3$, $sqrt(3) / 2$, $- 1 / 2$, $- sqrt(3)$,
    $135 degree$, $(3 pi) / 4$, $sqrt(2) / 2$, $- sqrt(2) / 2$, $-1$,
    $150 degree$, $(5 pi) / 6$, $1 / 2$, $- sqrt(3) / 2$, $- 1 / sqrt(3)$,
    $180 degree$, $pi$, $0$, $-1$, $0$,
  )
]

== Trigonometric Identities

#form()[
  === Doubled Angles

  $
    sin (2 alpha) &= 2 sin (alpha) cos (alpha) \
    cos ( 2 alpha) &= cos^2 (alpha) - sin^2 (alpha) = 1 - 2 sin^2 (alpha) \
    tan (2 alpha) &= (2 tan (alpha)) / (1 - tan^2 (alpha))
  $

  === Addition / Subtraction

  $
    sin (alpha plus.minus beta) &= sin (alpha) cos (beta) plus.minus cos (alpha) sin(beta) \
    cos (alpha plus.minus beta) &= cos (alpha) cos (beta) minus.plus sin (alpha) sin (beta) \
    tan (alpha plus.minus beta) &= (tan (alpha) plus.minus tan (beta)) / (1 minus.plus tan (alpha) tan (beta))
  $

  === Multiplication

  $
    sin (alpha) sin (beta) &= - (cos (alpha + beta) - cos (alpha - beta)) / 2 \
    cos (alpha) cos (beta) &= (cos (alpha + beta) + cos (alpha - beta)) / 2 \
    sin (alpha) cos (beta) &= (sin (alpha + beta) + sin (alpha - beta)) / 2
  $

  === Powers

  $
    sin^2 (alpha) &= 1 / 2 (1 - cos (2 alpha)) \
    sin^3 (alpha) &= (3 sin (alpha) - sin (3 alpha)) / 4 \
    cos^2 (alpha) &= 1 / 2 (1 + cos (2 alpha)) \
    cos^3 (alpha) &= (3 cos (alpha) - cos (3 alpha)) / 4 \
    tan^2 (alpha) &= (1 - cos ( 2 alpha)) / (1 + cos (2 alpha)) \
    sin^2 (alpha) cos^2 (alpha) &= (1 - cos (4 alpha)) / 8
  $

  === Divers

  $
    sin^2 (alpha) + cos^2 (alpha) &= 1 \
    cosh^2 (alpha) - sinh^2 (alpha) &= 1 \
    sin (z) &= (e^(i z) - e^(- i z)) / (2 i) \
    cos (z) &= (e^(i z) + e^(- i z)) / 2
  $
]

=== Midnight Formula

#form()[
  #columns(2)[
    $ x = (-b plus.minus sqrt(b^2 - 4 a c)) / (2 a) $
    #colbreak()
    $ b^2 - 4 a c < 0 \ => x "complex" $
  ]
]

=== Determinant

#form()[
  ==== *$2 times 2$*
  $ det (A) = |mat(a,c;b,d)| = a d - b c $
  ==== *$3 times 3$*
  $ det (A) = |mat(a,b,c;d,e,f;g,h,i)| = \ a e i + b f g + c d h - c e g - b d i - a f h $
  === *$n times n$*
  _Use recursion. Fix one element, eliminate it's row and column and calculate determinant from smaller remaining matrix. If row + column is even, multiply by $1$ else $-1$. Add all things together._
]

#form()[
  $
    det (I) &= 1 &&\
    det (A^T) &= det (A) &&\
    det (A^(-1)) &= 1 / (det (A)) &&\
    det (A B) &= det (A) det (B), space.quad &&A, B in RR^(n times n) \
    det (c A) &= c^n det (A), space.quad &&A in RR^(n times n) \
    det (A) &= a_11 a_22 dots.c a_(n n), space.quad &&A "triangular" \
  $
  #align(center)[
    one column / row is $0 => det (A) = 0$

    two rows / columns are equal $ => det (A) = 0$

    two rows / columns are swapped $=>$ sign switches
  ]
]

=== Trace

#form()[
  $
    tr (A) &= sum^n_(i = 1) a_(i i) = a_(1 1) + a_(2 2) + dots + a_(n n) \
    tr ( A + B) &= tr (A) + tr (B) \
    tr (c A) &= c tr (A) \
    tr (A) &= tr (A^T) \
  $
]

=== Eigenvalues

#form()[
  $A v = lambda v$, where $v$ is the *Eigenvector* and $lambda$ the *Eigenvalue*

  $ tr (A) = sum^n_(i=1) lambda_i = lambda_1 + lambda_2 + ... + lambda_n $

  $ det (A) = product^n_(i = 1) lambda_i = lambda_1 lambda_2 dots.c lambda_n $

  $ A "invertible" <=> "every eigenvalue is nonzero" $
]

=== Taylor Polynomials

#form()[
  $
    e^x &= 1 &&+ x + x^2 / 2 + x^3 / 3! + x^4 / 4! + O(x^5) \
    sin (x) &= x &&- x^3 / 3! + x^5 / 5! + O(x^7) \
    sinh (x) &= x &&+ x^3 / 3! + x^5 / 5! + O(x^7) \
    cos (x) &= 1 &&- x^2 / 2 + x^4 / 4! - x^6 / 6! + O(x^8) \
    cosh (x) &= 1 &&+ x^2 / 2 + x^4 / 4! + x^6 / 6! + O(x^8) \
    tan (x) &= x &&+ x^3 / 3 + (2 x^5) / 15 + O(x^7) \
    tanh (x) &= x &&- x^3 / 3 + (2 x^5) / 15 + O(x^7) \
    log (1 + x) &= x &&- x^2 / 2 + x^3 / 3 - x^4 / 4 + O(x^5) \
    (1 + x)^alpha &= 1 &&+ alpha x + (alpha (a - 1)) / (2 !) x^2 \ & &&+ (alpha (alpha - 1)(alpha - 2)) / (3!) x^3 + O(x^4) \
    sqrt(1+x) &= 1 &&+ x / 2 - x^2 / 8 + x^3 / 16 - O(x^4) \
    e^(-x) &= 1 &&- x + x^2 / 2 - x^3 / 6 + x^4 / 24 + O(x^5)
  $
]

=== Logarithm Rules

#form()[
  $
    log_b (x y) &= log_b (x) + log_b (y) \
    log_b (x / y) &= log_b (x) - log_b (y) \
    log_b (x^p) &= p log_b (x) \
    log_b (root(p, x)) &= (log_b (x)) / p \
    log_b (x) &= (log_k (x)) / (log_k (b))
  $
]

=== Differentiation Rules

#form()[
  $
    (a f plus.minus b g)' &= a f' plus.minus b g' \
    (f g)' (x) &= f' (x) g(x) + f (x) g' (x) \
    (f (g (x)))' &= f' (g (x)) dot g' (x) \
    g' &= 1 / (f' compose g), space.quad g "inverse of" f \
    (1 / f(x))' &= - (f' (x)) / (f (x))^2 \
    (f / g)' &= (f' g - g' f) / g^2
  $
]

=== Integration Rules

#form()[
  $
    F(x) &= integral_a^x f(t) d t, space.quad F'(x) = f(x) \
    integral^b_a f(x) d x &= F(b) - F(a) \
    integral (a f plus.minus b g) d x &= a integral f d x plus.minus b integral g d x \
    integral x^n d x &= (x^(n+1)) / (n + 1) + C \
    integral f dot g' d x &= f dot g - integral f' g d x \
    F compose Phi (u) &= integral f(Phi (u)) Phi ' (u) d u \
    f(-x) = f(x) &=> integral^a_(-a) f(x) d x = 2 integral_0^a f(x) d x \
    f(-x) = -f(x) &=> integral^a_(-a) f(x) d x = 0 \
  $
]

=== Differentials / Integrals

#form()[
  #table(
    columns: (1fr, 1fr),
    table.header([*$F(x)$*], [*$F'(x) = f(x)$*]),
    stroke: (x, y) => {
      if y == 0 {
        (bottom: 0.7pt + black)
      }
      if (x == 0) {
        (right: 0.7pt + black)
      }
    },
    $c$, $0$,
    $x^a$, $a dot x^(a-1)$,
    $1 / (a + 1) x^(a+1)$, $x^a$,
    $1 / (a dot (n+1)) (a x + b)^(n+1)$, $(a x + b)^n$,
    $(x^(alpha + 1)) / (alpha + 1)$, $x^alpha , space alpha != -1$,
    $sqrt(x)$, $1 / (2 sqrt(x))$,
    $root(n, x)$, $1 / n x^(1 / n - 1)$,
    $2 / 3 x^(2 / 3)$, $sqrt(x)$,
    $n / (n+1) x^(1 / n + 1)$, $root(n, x)$,
    $e^x$, $e^x$,
    $ln (|x|)$, $1 / x$,
    $log_a (|x|)$, $1 / (x ln (a)) = log_a (e) 1 / x$,
    $sin (x)$, $cos (x)$,
    $cos (x)$, $- sin(x)$,
    $tan (x)$, $1 / (cos^2 (x)) = 1 + tan^2 (x)$,
    $cot (x)$, $1 / (- sin^2 (x))$,
    $arcsin (x)$, $1 / sqrt(1 - x^2)$,
    $arccos (x)$, $-1 / sqrt(1 - x^2)$,
    $arctan (x)$, $1 / (1 + x^2)$,
    $sinh (x)$, $cosh (x)$,
    $cosh (x)$, $sinh (x)$,
    $tanh (x)$, $1 / (cosh^2 (x)) = 1 - tanh^2 (x)$,
    $"arcsinh" (x)$, $1 / sqrt(1+x^2)$,
    $"arccosh" (x)$, $1 / sqrt(x^2 - 1)$,
    $"arctanh" (x)$, $1 / (1-x^2)$,
    $1 / f(x)$, $(- f' (x)) / ((f(x))^2)$,
    $a^(c x)$, $a^(c x) dot c ln (a)$,
    $x^x$, $x^x dot (1 + ln (x))_( x > 0)$,
    $(x^x)^x$, $(x^x)^x (x + 2 x ln (x))_(x > 0)$,
    $x^((x^x))$, $x^((x^x)) dot \ (&x^(x-1) + ln (x) dot \ &x^x (1 + ln (x)))_(x > 0)$,
    $1 / a ln (a x + b)$, $1 / (a x + b)$,
    $(a x) / c - (a d - b c) / c^2 ln (| c x + d|)$, $(a x + b) / (c x + d)$,
    $1 / (2 a) ln (| (x - a) / (x + a)|)$, $1 / (x^2 - a^2)$,
    $x / 2 f(x) + a^2 / 2 ln (x + f(x))$, $sqrt(a^2 + x^2)$,
    $x / 2 sqrt(a^2 - x^2) - a^2 / 2 arcsin (x / (|a|))$, $sqrt(a^2 - x^2)$,
    $x / 2 f(x) - a^2 / 2 ln (x + f(x))$, $sqrt(x^2 - a^2)$,
    $ln(x + sqrt(x^2 plus.minus a^2))$, $1 / sqrt(x^2 plus.minus a^2)$,
    $arcsin (x / (|a|))$, $1 / sqrt(a^2 - x^2)$,
    $1 / a arctan (x / a)$, $1 / (x^2 + a^2)$,
    $- 1 / a cos (a x + b)$, $sin (a x + b)$,
    $1 / a sin (a x + b)$, $cos (a x + b)$,
    $- ln (|cos (x)|)$, $tan (x)$,
    $ln(|sin (x)|)$, $cot (x)$,
    $ln (| tan ( x / 2) |)$, $1 / (sin (x))$,
    $ln (| tan (x / 2 + pi / 4) | )$, $1 / (cos (x))$,
    $1 / 2 (x - sin (x) cos (x))$, $sin^2 (x)$,
    $1 / 2 (x + sin (x) cos(x))$, $cos^2 (x)$,
    $tan (x)- x$, $tan^2 (x)$,
    $- cot (x) - x$, $cot^2 (x)$,
    $x arcsin (x) + sqrt( 1 - x^2)$, $arcsin (x)$,
    $x arccos (x) - sqrt(1 - x^2)$, $arccos (x)$,
    $x arctan (x) - 1 / 2 ln (1 + x^2)$, $arctan (x)$,
    $ln (cosh (x))$, $tanh (x)$,
    $ln ( |f (x)|)$, $(f'(x)) / (f(x))$,
    $x dot (ln (|x|) -1)$, $ln (|x|)$,
    $1 / (n+1) (ln (x))^(n+1)_(n != -1)$, $1 / x (ln (x))^n$,
    $1 / (2n) (ln (x^n))^2_(n != 0)$, $1 / x ln (x^n)$,
    $ln (|ln (x) |)_(x > 0, x != 1)$, $1 / (x ln(x))$,
    $1 / (b ln(a)) a^(b x)$, $a^(b x)$,
    $(c x - 1) / (c^2) dot e^(c x)$, $x dot e^(c x)$,
    $(x^(n + 1)) / (n + 1) (ln (x) - 1 / (n + 1))_(n != -1)$, $x^n ln(x)$,
    $(e^(c x) (c sin (a x + b) - a cos (a x + b))) / (a^2 + c^2)$, $e^(c x) sin (a x + b)$,
    $(e^(c x) (c cos (a x + b) + a sin (a x + b))) / (a^2 + c^2)$, $e^(c x) cos ( a x + b)$,
    $(sin^2 (x)) / 2$, $sin (x) cos (x)$
  )
]

=== Binomial Formulas

#form()[
  $
    (a + b)^2 &= a^2 + 2 a b + b^2 \
    (a - b)^2 &= a^2 - 2 a b + b^2 \
    (a + b) (a - b) &= a^2 - b^2
  $
]

==== Pascal's Triangle

// i wanted to do something fun. this was very fun indeed
#form()[
  #align(
    center,
    {
      let r = 0
      while r <= 6 {
        let c = 0
        while (c <= r) {
          [$#calc.binom(r, c) space.quad$]
          c += 1
        }
        [ \ ]
        r += 1
      }
    },
  )
]

=== Bijection, Injection and Surjection

#form()[
  / Injective: Every $x$ as a unique $y$
  / Surjective: Every $y$ has a unique $x$
  / Bijective: Injective and Surjective
]

=== Binomial Coefficient

#form()[
  $ vec(n, k) = (n!) / (k! (n - k)!) $
]

=== Constants

#form()[
  $
    pi &= #calc.pi \
    e &= #calc.e
  $
]
