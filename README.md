
# `{rastro}`

`{rastro}` is an attempt to explore the new type system provided by the
`{vctrs}` package. In `{rastro}` I create some simple data containers
for simple scenarios that I encounter in my scientific work.

Consider the following example. When an observation is carried out,
obtained value is usually accompanied by an error. The error can be
symmetric or asymmetric. This can be represented in a `tibble` using two
or three columns. Alternatively, a special type can be created that
stores observations and errors and can be easily transformed to and from
`data.frame`.

``` r
library(rastro)
library(tibble, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Create an observation from a standalone value
new_obs(5)
```

    ## <rastro_obs<double>[1]>
    ## [1] 5 ± 0

``` r
# Add an optional error
new_obs(rnorm(5), runif(5, 0, 0.5))
```

    ## <rastro_obs<double>[5]>
    ## [1] -0.2077008 ± 0.0004374204 -0.2105487 ± 0.0237322067
    ## [3] -1.5114791 ± 0.1428552294  1.3983632 ± 0.0793717374
    ## [5]  1.2839821 ± 0.4446579374

``` r
# And asymmetric errors
new_obs(1:10, p_err = 2, n_err = 1)
```

    ## <rastro_obs<double>[10]>
    ##  [1]  1 (- 1; + 2)  2 (- 1; + 2)  3 (- 1; + 2)  4 (- 1; + 2)  5 (- 1; + 2)
    ##  [6]  6 (- 1; + 2)  7 (- 1; + 2)  8 (- 1; + 2)  9 (- 1; + 2) 10 (- 1; + 2)

With proper methods defined, `rastro_obs` can be used within `tibble`:

``` r
tibble(Time = 1:10, Value = new_obs(rnorm(10), runif(10, 0.1, 0.5)))
```

    ## # A tibble: 10 x 2
    ##     Time                  Value
    ##    <int>             <obs<dbl>>
    ##  1     1  0.7565622 ± 0.2805280
    ##  2     2 -0.5283559 ± 0.3997558
    ##  3     3 -0.4247370 ± 0.1964134
    ##  4     4  0.4524013 ± 0.1073089
    ##  5     5 -0.4844491 ± 0.2554091
    ##  6     6 -0.8288113 ± 0.4499621
    ##  7     7 -1.3432404 ± 0.1895942
    ##  8     8 -0.2807079 ± 0.2458426
    ##  9     9  0.8926537 ± 0.2598893
    ## 10    10 -1.2956212 ± 0.2678528

`rastro_obs<T>` behaves like a generic of type `T`. Type conversion
rules limit possible values of `T`, but this limitation can be lifted by
simply implementing additional type-conversion rules.

``` r
# An integer observation
new_obs(5L)
```

    ## <rastro_obs<integer>[1]>
    ## [1] 5 ± 0

``` r
# Strings are not allowed
new_obs("a")
```

    ## Error: Can't convert from <double> to <character>.

Arithmetic can be also implemented. A simple scenario would be to
multiply/divide an observed quantity by a scalar, which scales both
observation and its error:

``` r
new_obs(1:10, 4) * 0.5
```

    ## <rastro_obs<double>[10]>
    ##  [1] 0.5 ± 2 1.0 ± 2 1.5 ± 2 2.0 ± 2 2.5 ± 2 3.0 ± 2 3.5 ± 2 4.0 ± 2 4.5 ± 2
    ## [10] 5.0 ± 2

Another useful case is manipulation of multiple observed quantities. For
instance, subtracting some background/baseline estimate (which itself is
not known precisely) is a common task. To get the error on the derived
quantity, a simple error propagation rule can be applied. The
application of such rule can be encoded into the arithmetic functions
for such type:

``` r
new_obs(30, 3) - new_obs(10, 4)
```

    ## <rastro_obs<double>[1]>
    ## [1] 20 ± 5

The result is `20 ± 5`. If the function is
<img src="https://latex.codecogs.com/gif.latex?f(x,y)&space;=&space;x&space;-&space;y" title="f(x,y) = x - y" />
Then
<img src="https://latex.codecogs.com/gif.latex?\sigma_f&space;=&space;\sqrt{\left(\dfrac{\partial&space;f}{\partial&space;x}\sigma_x\right)^2&space;&plus;&space;\left(\dfrac{\partial&space;f}{\partial&space;y}\sigma_y\right)^2}" title="\sigma_f = \sqrt{\left(\dfrac{\partial f}{\partial x}\sigma_x\right)^2 + \left(\dfrac{\partial f}{\partial y}\sigma_y\right)^2}" />,
which is simply
<img src="https://latex.codecogs.com/gif.latex?\sqrt{\sigma_x^2&space;&plus;&space;\sigma_y^2}" title="\sqrt{\sigma_x^2 + \sigma_y^2}" />
for a subtraction.

## A more complex scenario

A similar type can be devised for a physical quantity. I chose flux
(energy per unit area and unit time). Flux measurements are associated
with a “filter” and a zero-point flux value (necessary for conversion).
In general, it is illegal to combine fluxes obtained in different
filters.

``` r
new_flux(100, "B", "mJy")
```

    ## <rastro_flux<B>[1]>
    ## [1] 1.000e+02
    ## Unit: mJy

This gives us flux of `100` milliJansky measured in filter `B`. If the
error on this measurement is, say, `5 mJy`, it is possible to store it
as

``` r
new_obs(new_flux(100, "B", "mJy"), new_flux(5, "B", "mJy"))
```

    ## <rastro_obs<rastro_flux<B>>[1]>
    ## [1] 1.000e+02 ± 5.000e+00

In some cases the type inference can kick in and the error can be simply
input as a `double`. This system prevents operation on fluxes obtained
from different filters:

``` r
new_obs(new_flux(100, "B")) - new_obs(new_flux(50, "R"))
```

    ## Error: No common type for `..1` <rastro_flux<B>> and `..2` <rastro_flux<R>>.

But allows operations for the same filter (but not within
`rastro_obs<T>`, because it requires `T` to be squared and then to
extract square root from the sum)

``` r
new_flux(100, "B") - new_flux(50, "B")
```

    ## <rastro_flux<B>[1]>
    ## [1] 5.000e+01
    ## Unit: NA

## Some more utilities

Angle measurements can be tricky, especially if angles are measured in
degrees. Here is a solution

``` r
new_degr(c(370, -370))
```

    ## <rastro_degr[2]>
    ## [1] 10°  350°

Values are mapped to `[0;360]` interval. Base function support can be
also defined:

``` r
mean(new_degr(10:30))
```

    ## <rastro_degr[1]>
    ## [1] 20°

And, most importantly,

``` r
sin(new_degr(30))
```

    ## [1] 0.5

``` r
cos(new_degr(60))
```

    ## [1] 0.5

**Work is still in progress**
