Writing Function
================
Rachel
11/11/2021

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.44264869  1.64255607 -1.44541442 -0.38066699 -0.33674837  0.52369557
    ##  [7] -1.00369881  0.97549593 -1.51416616  2.10094821  0.45374711 -0.38209468
    ## [13]  1.12431058 -0.24294055  0.60287161  0.50016994  0.72134842  0.03142142
    ## [19] -2.27048702 -0.68671449 -0.41713040 -0.75237997  0.93034379 -0.49981177
    ## [25] -0.75988462  0.03672122 -1.01477916 -0.25649136 -0.10099030  0.97812051

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

    ##  [1]  1.44264869  1.64255607 -1.44541442 -0.38066699 -0.33674837  0.52369557
    ##  [7] -1.00369881  0.97549593 -1.51416616  2.10094821  0.45374711 -0.38209468
    ## [13]  1.12431058 -0.24294055  0.60287161  0.50016994  0.72134842  0.03142142
    ## [19] -2.27048702 -0.68671449 -0.41713040 -0.75237997  0.93034379 -0.49981177
    ## [25] -0.75988462  0.03672122 -1.01477916 -0.25649136 -0.10099030  0.97812051

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

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three number")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )

  
}
```

Check that the function works

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.52  3.96

## Multiple inputs

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.77  2.84

``` r
sim_mean_sd = function(sample_size, mu, sigma){
  sim_data = 
  tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.30  3.55

``` r
sim_mean_sd = function(sample_size, mu = 3 , sigma = 4){
  sim_data = 
  tibble(
    x = rnorm(n = sample_size, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(sample_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.48  2.93

``` r
sim_mean_sd( mu = 6, sample_size = 100, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.46  2.97

``` r
sim_mean_sd(sample_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.91  3.76
