Writing Function
================
Rachel
11/11/2021

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.1253291  1.5455450 -0.2931794  0.7423581 -0.7744403 -0.1736961
    ##  [7]  1.1448613  0.5980797 -0.6754189 -1.1361438  1.1076469 -0.4348279
    ## [13] -0.6096467  0.3074271 -0.7684054  0.3846010 -0.2347013  0.6077729
    ## [19] -0.7114101  2.4903374 -1.1251376 -1.4649561 -0.4740586  1.4942400
    ## [25] -1.7496696  0.6754614  0.1716820 -0.1623592  0.8217392 -1.4290304

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

    ##  [1]  0.1253291  1.5455450 -0.2931794  0.7423581 -0.7744403 -0.1736961
    ##  [7]  1.1448613  0.5980797 -0.6754189 -1.1361438  1.1076469 -0.4348279
    ## [13] -0.6096467  0.3074271 -0.7684054  0.3846010 -0.2347013  0.6077729
    ## [19] -0.7114101  2.4903374 -1.1251376 -1.4649561 -0.4740586  1.4942400
    ## [25] -1.7496696  0.6754614  0.1716820 -0.1623592  0.8217392 -1.4290304

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
    ## 1  3.42  4.36

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
    ## 1  3.89  2.87

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
    ## 1  6.79  3.07

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
    ## 1  5.85  2.99

``` r
sim_mean_sd( mu = 6, sample_size = 100, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.02  2.72

``` r
sim_mean_sd(sample_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.05  3.80

## Let’s review Nopoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews = function(url){
  

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
}
```

What about the next page of review

Let’s turn that code into a function

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews = function(url){
  

 html = read_html(url)

review_titles = 
  html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
 html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews

}
```

Let me try my function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 × 3
    ##    title                             stars text                                 
    ##    <chr>                             <dbl> <chr>                                
    ##  1 hehehehe                              5 goodjobboys                          
    ##  2 Painful                               1 I think I sneezed during the movie a…
    ##  3 GRAND                                 5 GRAND                                
    ##  4 Hello, 90s                            5 So nostalgic movie                   
    ##  5 Cult Classic                          5 Watched it with my older grandchilde…
    ##  6 Format was inaccurate                 4 There was an option to choose blue R…
    ##  7 Good funny                            3 Would recommend                      
    ##  8 Not available w/in 48 hour window     1 I couldn't watch it and there is no …
    ##  9 Your mom went to college.             5 Classic funny movie. It has some of …
    ## 10 Very funny movie                      5 I watch this movie with my family. V…

Let’s read a few pages of reviews

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:50)

all_reviews = 
  bind_rows(
  read_page_reviews(dynamite_urls[1]),
  read_page_reviews(dynamite_urls[2]),
  read_page_reviews(dynamite_urls[3]),
  read_page_reviews(dynamite_urls[4]),
  read_page_reviews(dynamite_urls[5])
)
```

## mean scoping example

``` r
f =function(x) {
  z = x + y 
  z
}

x = 1 
y = 2

f(x=y)
```

    ## [1] 4
