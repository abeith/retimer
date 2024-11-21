
data(mm1)

test_that("spectrogram throws error with vector for x and no fs argument", {
    expect_error(spectrogram(1:3))
})

test_that("spectrogram throws error with file path that isn't mp3 or mp4", {
    expect_error(spectrogram("./file.mp4"))
})

test_that("spectrogram throws error when trying to use a matrix", {
    expect_error(spectrogram(matrix(mm1@left, ncol = 1)))
})

## Check spectrogram dimensions

seewave_spec <- spectrogram(mm1, method = "seewave", output = "list")

test_that("seewave spectrogram has expected dimensions", {
    expect_length(seewave_spec$time, 263)
    expect_length(seewave_spec$freq, 200)
    expect_equal(nrow(seewave_spec$amp), 263)
    expect_equal(ncol(seewave_spec$amp), 200)
})

phontools_spec <- spectrogram(mm1, method = "phonTools", output = "list")

test_that("phontools spectrogram has expected dimensions", {
    expect_length(phontools_spec$time, 261)
    expect_length(phontools_spec$freq, 2201)
    expect_equal(nrow(phontools_spec$amp), 261)
    expect_equal(ncol(phontools_spec$amp), 2201)
})

tuner_spec <- spectrogram(mm1, method = "tuneR", output = "list")

test_that("tuner spectrogram has expected dimensions", {
    expect_length(tuner_spec$time, 262)
    expect_length(tuner_spec$freq, 256)
    expect_equal(nrow(tuner_spec$amp), 262)
    expect_equal(ncol(tuner_spec$amp), 256)
})

gsignal_spec <- spectrogram(mm1, method = "gsignal", output = "list")

test_that("gsignal spectrogram has expected dimensions", {
    expect_length(gsignal_spec$time, 262)
    expect_length(gsignal_spec$freq, 200)
    expect_equal(nrow(gsignal_spec$amp), 262)
    expect_equal(ncol(gsignal_spec$amp), 200)
})

## But why these dimensions?
## Time makes sense with different implementations

test_that("timesteps are equal or close to expected value", {
    expect_equal(mean(diff(phontools_spec$time)), 0.01)
    expect_equal(mean(diff(tuner_spec$time)), 0.01)
    expect_equal(mean(diff(seewave_spec$time)), 0.01, tolerance = 1e-2)
    expect_equal(mean(diff(gsignal_spec$time)), 0.01)
})

## Phontools saves dimensions as strings resulting in rounding errors in bins
## This can cause artefacts when plotting

phontools_freq <- (phontools_spec$freq)

test_that("frequency bins are linear", {
    expect_identical(seq(min(tuner_spec$freq), max(tuner_spec$freq), length.out = length(tuner_spec$freq)), tuner_spec$freq)
    expect_identical(seq(min(seewave_spec$freq), max(seewave_spec$freq), length.out = length(seewave_spec$freq)), seewave_spec$freq)
    expect_identical(seq(min(phontools_spec$freq), max(phontools_spec$freq), length.out = length(phontools_spec$freq)), phontools_spec$freq)
})

seewave_tbl <- spectrogram(mm1, method = 'seewave', output = 'tibble')
phontools_tbl <- spectrogram(mm1, method = 'phontools', output = 'tibble')
tuner_tbl <- spectrogram(mm1, method = 'tuner', output = 'tibble')

test_that("tibbles are expected dimensions", {
    expect_equal(nrow(seewave_tbl), 52600)
    expect_equal(nrow(phontools_tbl), 574461)
    expect_equal(nrow(tuner_tbl), 67072)
})

## Check that I can recreate defaults with the spectrogram function

phontools_default <- function(wav) {
    phontools_spec <- phonTools::spectrogram(wav@left, wav@samp.rate, show = F)
    spec <- list()
    spec$time <- as.numeric(attr(phontools_spec$spectrogram, "dimnames")[[1]])/1e3
    spec$freq <- as.numeric(attr(phontools_spec$spectrogram, "dimnames")[[2]]) |> retimer:::regularise_vector()
    spec$amp <- phontools_spec$spectrogram  
    tbl <- retimer:::spec2tbl(spec)
    return(tbl)
}

seewave_default <- function(wav) {
    spec <- seewave::spectro(wav@left, wav@samp.rate, plot = F)
    spec$amp <- t(spec$amp)
    spec$freq <- spec$freq * 1e3
    tbl <- retimer:::spec2tbl(spec)
    return(tbl)
}

gsignal_default <- function(wav) {
    gspec <- gsignal::specgram(wav@left, fs = wav@samp.rate)
    spec <- list()
    spec$amp <- gspec$S |> t() |> Mod()
    spec$amp <- 20 * log10(spec$amp)
    spec$freq <- gspec$f
    spec$time <- gspec$t
    tbl <- retimer:::spec2tbl(spec)
    return(tbl)
}

test_that("Package defaults can be achieved with wrapper", {
    ## Phontools uses a wintime of 5ms and divides into 1000 steps
    expect_identical(
        phontools_default(mm1),
        spectrogram(mm1, method = 'phontools', wintime = 5, steptime = length(mm1) / mm1@samp.rate))
    ## Seewave uses 512 samples for window length and step (0% overlap)
    expect_identical(
        seewave_default(mm1),
        spectrogram(mm1, method = 'seewave', wintime = 1e3 * 512 / mm1@samp.rate, steptime = 1e3 * 512 / mm1@samp.rate))
    ## tuneR uses a wintime of 0.025s and a steptime of 0.01s
    ## Testing this would be identical to the implementation
    ## gsignal
    expect_identical(
        gsignal_default(mm1),
        spectrogram(mm1, method = 'gsignal', wintime = 1e3 * 256 / mm1@samp.rate, steptime = 1e3 * 128 / mm1@samp.rate))
})


###############
## Debugging ##
###############

## Visual check
## Error in seewave method? Looks like accumulated rounding error

## Check if the defaults of any package defaults give correct alignment

## library(tidyverse)

## fs <- mm1@samp.rate

## ## With seewave defaults
## wintime <- 1e3 * 512 / mm1@samp.rate
## steptime <- 1e3 * 512 / mm1@samp.rate
## list(seewave = 'seewave', phontools = 'phontools', tuner = 'tuner', gsignal = 'gsignal') |>
##     map(\(method) spectrogram(mm1, method = method, wintime = wintime, steptime = steptime)) |>
##     bind_rows(.id = 'method') |>
##     mutate(method = factor(method, levels = c('seewave', 'phontools', 'tuner', 'gsignal'))) |>
##     group_by(method) |>
##     mutate(amp = amp - min(amp),
##            amp = amp / max(amp)) |>
##     ungroup() |>
##     ggplot() +
##     geom_raster(aes(t, f, fill = amp), alpha = 0.9, show.legend = F) +
##     geom_vline(xintercept = length(mm1) / fs, colour = 'red') +
##     scale_x_continuous(expand = c(0,0),
##                        breaks = seq_len(500) * steptime * 2e-3,
##                        minor_breaks = seq_len(1000) * steptime * 1e-3, labels = \(x) sprintf("%0.3f", x)
##                        ) +
##     scale_y_continuous(expand = c(0,0)) +
##     coord_cartesian(xlim = c(2.5, 2.66)) +
##     facet_wrap(. ~ method, nrow = 4) +
##     scale_fill_viridis_c()

## plot(1)

## devtools::load_all()
## ## With phontools defaults
## wintime <- 5
## steptime <- length(mm1) / mm1@samp.rate
## list(seewave = 'seewave', phontools = 'phontools', tuner = 'tuner', gsignal = 'gsignal') |>
##     map(\(method) spectrogram(mm1, method = method, wintime = wintime, steptime = steptime)) |>
##     bind_rows(.id = 'method') |>
##     mutate(method = factor(method, levels = c('seewave', 'phontools', 'tuner', 'gsignal'))) |>
##     group_by(method) |>
##     mutate(amp = amp - min(amp),
##            amp = amp / max(amp)) |>
##     ungroup() |>
##     ggplot() +
##     geom_raster(aes(t, f, fill = amp), alpha = 0.9, show.legend = F) +
##     geom_vline(xintercept = length(mm1) / fs, colour = 'red') +
##     scale_x_continuous(expand = c(0,0),
##                        breaks = seq_len(500) * steptime * 2e-3,
##                        minor_breaks = seq_len(1000) * steptime * 1e-3, labels = \(x) sprintf("%0.3f", x)
##                        ) +
##     scale_y_continuous(expand = c(0,0)) +
##     coord_cartesian(xlim = c(2.5, 2.66)) +
##     facet_wrap(. ~ method, nrow = 4) +
##     scale_fill_viridis_c()


## ## With tuner defaults
## wintime <- 25
## steptime <- 10
## list(seewave = 'seewave', phontools = 'phontools', tuner = 'tuner', gsignal = 'gsignal') |>
##     map(\(method) spectrogram(mm1, method = method, wintime = wintime, steptime = steptime)) |>
##     bind_rows(.id = 'method') |>
##     mutate(method = factor(method, levels = c('seewave', 'phontools', 'tuner', 'gsignal'))) |>
##     group_by(method) |>
##     mutate(amp = amp - min(amp),
##            amp = amp / max(amp)) |>
##     ungroup() |>
##     ggplot() +
##     geom_raster(aes(t, f, fill = amp), alpha = 0.9, show.legend = F) +
##     geom_vline(xintercept = length(mm1) / fs, colour = 'red') +
##     scale_x_continuous(expand = c(0,0),
##                        breaks = seq_len(500) * steptime * 2e-3,
##                        minor_breaks = seq_len(1000) * steptime * 1e-3, labels = \(x) sprintf("%0.3f", x)
##                        ) +
##     scale_y_continuous(expand = c(0,0)) +
##     coord_cartesian(xlim = c(2.5, 2.66)) +
##     facet_wrap(. ~ method, nrow = 4) +
##     scale_fill_viridis_c()

## ## With gsignal defaults
## wintime <- 1e3 * 256 / mm1@samp.rate
## steptime <- 1e3 * 128 / mm1@samp.rate
## list(seewave = 'seewave', phontools = 'phontools', tuner = 'tuner', gsignal = 'gsignal') |>
##     map(\(method) spectrogram(mm1, method = method, wintime = wintime, steptime = steptime)) |>
##     bind_rows(.id = 'method') |>
##     mutate(method = factor(method, levels = c('seewave', 'phontools', 'tuner', 'gsignal'))) |>
##     group_by(method) |>
##     mutate(amp = amp - min(amp),
##            amp = amp / max(amp)) |>
##     ungroup() |>
##     ggplot() +
##     geom_raster(aes(t, f, fill = amp), alpha = 0.9, show.legend = F) +
##     geom_vline(xintercept = length(mm1) / fs, colour = 'red') +
##     scale_x_continuous(expand = c(0,0),
##                        breaks = seq_len(500) * steptime * 2e-3,
##                        minor_breaks = seq_len(1000) * steptime * 1e-3, labels = \(x) sprintf("%0.3f", x)
##                        ) +
##     scale_y_continuous(expand = c(0,0)) +
##     coord_cartesian(xlim = c(2.5, 2.66)) +
##     facet_wrap(. ~ method, nrow = 4) +
##     scale_fill_viridis_c()


## Seewave aligns end of last window with length of audio file in all cases. phonTools and tuneR do not

## ## Confirm that phonTools doesn't deal with error in plot method
## spec <- phonTools::spectrogram(mm1@left, mm1@samp.rate, show = F)
## par(mfrow = c(2,1))
## plot(spec, xlim = c(2500, 2650))
## abline(v = 1e3 * length(mm1) / fs, col = 'red')
## with(spectrogram(mm1, method = 'phontools', output = 'list', wintime = 5, steptime = length(mm1) / mm1@samp.rate),
##      image(time, freq, amp, col = hcl.colors(20), xlim = c(2.5, 2.65)))
## abline(v = length(mm1) / fs, col = 'red')

