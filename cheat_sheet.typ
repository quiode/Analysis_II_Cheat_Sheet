
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
  Let $f in C^2$. Let $x_0$ be a non-degenerate critical point of $f$. Let $p$ and $q$ be the number of positive and negative eigenvalues of $"Hess"_f (x_0)$.
  + If $p = n$, equivalently if $q = 0$, the function $f$ has a *local minimum* at $x_0$.
  + If $q = n$, equivalently if $p = 0$, the function $f$ has a *local maximum* at $x_0$.
  + Otherwise, equivalent if $p q != 0$, the function $f$ does not have a local extremum at $x_0$. One then says that $f$ has a *saddle point* at $x_0$.
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

== Formula Collection

#text(red)[TODO!]
