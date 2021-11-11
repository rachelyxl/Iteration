Writing Function
================
Rachel
11/11/2021

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.7545589  0.3954001 -0.7811574  0.8605955 -1.9354778  0.4040495
    ##  [7] -2.3117936 -0.2010518  0.2087300  0.9448761  0.4635059  0.2421753
    ## [13] -0.7498993  0.4909276 -0.1037403  1.0423705  0.0943206  0.4568040
    ## [19]  0.5329007  0.1265098 -0.6042480 -1.1758147  0.2611813  0.4460201
    ## [25]  1.9151418 -0.3380044 -1.2587143 -1.4439870  1.9740040  0.7989349

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

    ##  [1] -0.7545589  0.3954001 -0.7811574  0.8605955 -1.9354778  0.4040495
    ##  [7] -2.3117936 -0.2010518  0.2087300  0.9448761  0.4635059  0.2421753
    ## [13] -0.7498993  0.4909276 -0.1037403  1.0423705  0.0943206  0.4568040
    ## [19]  0.5329007  0.1265098 -0.6042480 -1.1758147  0.2611813  0.4460201
    ## [25]  1.9151418 -0.3380044 -1.2587143 -1.4439870  1.9740040  0.7989349

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
    ## 1  3.42  3.96

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
    ## 1  4.32  3.17

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
    ## 1  5.99  3.10

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
    ## 1  5.99  2.67

``` r
sim_mean_sd( mu = 6, sample_size = 100, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.97  2.92

``` r
sim_mean_sd(sample_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.62  3.60

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
f = function(x) {
  z = x + y 
  z
}

x = 1 
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, sum_func) {
  
  sum_func(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 3.610553

``` r
median(x_vec)
```

    ## [1] 3.525123

``` r
my_summary(x_vec, IQR)
```

    ## [1] 9.155352
