Writing Function
================
Rachel
11/11/2021

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.05010362 -2.10505966 -0.69890374  1.25997409  0.82637370  2.09454292
    ##  [7]  0.38644626 -1.58969264  1.22791231  0.11199425  0.37050517 -1.34195392
    ## [13]  0.34704982  0.74277494 -0.09957405 -0.12382693  0.52040566  0.08552396
    ## [19]  0.26639691 -0.41128992 -2.12716915  0.68089813  0.06355076 -0.74392545
    ## [25]  0.24661949 -1.56123950  1.09055438  0.01952309  0.84948806 -0.33779532

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three number")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.05010362 -2.10505966 -0.69890374  1.25997409  0.82637370  2.09454292
    ##  [7]  0.38644626 -1.58969264  1.22791231  0.11199425  0.37050517 -1.34195392
    ## [13]  0.34704982  0.74277494 -0.09957405 -0.12382693  0.52040566  0.08552396
    ## [19]  0.26639691 -0.41128992 -2.12716915  0.68089813  0.06355076 -0.74392545
    ## [25]  0.24661949 -1.56123950  1.09055438  0.01952309  0.84948806 -0.33779532

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three number

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, FALSE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, FALSE)): Input must be numeric
