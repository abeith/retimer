
<!-- README.md is generated from README.Rmd. Please edit that file -->

# retimer

<!-- badges: start -->
<!-- badges: end -->

The `retimer` package provides tools for retiming and analysis of
speech.

## Installation

You can install the development version of retimer from
[GitHub](https://github.com/):

``` r
remotes::install_github("abeith/retimer")
```

## Usage

### WSOLA

The WSOLA (Wave Similarity Overlap-Add) algorithm for performing
retimings is implemented as a native R function.

For example, to create a random retiming of the included `mm1` Wave
object:

``` r

library(retimer)

## Load example data
data(mm1)

## Find the length (in samples) of the object
dur <- length(mm1@left)

## Set the number of anchors to use
n <- 10

## Sample some random interval durations
x <- runif(n)

## Make a list of input output anchors
anchors <- list(anc_in = c(0, dur*seq_len(n)/n),
                anc_out = c(0, dur*cumsum(x)/sum(x)))

## Run the retiming
sig <- wsola(mm1@left, anchors)

## Create a new Wave object with the retimed signal
wav <- tuneR::Wave(sig, samp.rate = mm1@samp.rate, bit = mm1@bit)

## Listen to the retimed audio
tuneR::play(wav, 'play')
```

### Praat OLA

The `praatRetime` function is used to perform a retiming in Praat with
the [overlap-add
method](https://www.fon.hum.uva.nl/praat/manual/overlap-add.html). To
use this function and other Praat functions in the `retimer` package,
you must have Praat installed and available in your PATH. Running the
`praatSys` function with no arguments should output the Version number
of your Praat installation.

``` r

praatSys()
```

To create a similar retiming to the above `wsola` example, it is
necessary to create a nested tibble that can be converted to a TextGrid
with the first tier indicating the existing timing and the second tier
indicating the desired timing.

``` r

library(retimer)
library(tidyverse)

## Load example data
data(mm1)

## Find the length (in seconds) of the object
dur <- length(mm1)/mm1@samp.rate

## Set the number of anchors to use
x <- runif(10)

## Define the ends of the intervals for the output tier
t2_out <- dur*cumsum(x)/sum(x)
## Define the starts of the intervals for the output tier
t1_out <- c(0, t2_out[-length(t2_out)])
## Define the ends of the intervals for the input tier
t2_in <- dur*seq_len(10)/10
## Define the starts of the intervals for the input tier
t1_in <- c(0, t2_in[-length(t2_in)])

## Create a TextGrid tibble
tg <- tibble(
    name = rep(c("old", "new"), each = 10),
    type = "interval",
    t1 = c(t1_in, t1_out),
    t2 = c(t2_in, t2_out),
    label = rep(letters[1:10], times = 2)) |>
    nest(data = c(t1, t2, label))

## Run the retiming
wav_retimed <- praatRetime(mm1, tg)

## Listen to the retimed audio
tuneR::play(wav_retimed, 'play')
```
