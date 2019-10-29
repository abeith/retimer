# Compare 12-TET to bark

library(emuR)

dat <- tibble(f = seq(50, 20000, 1)) %>%
  mutate(tones = freqToTones(f, 50),
         bark = bark(f))

f_bark <- ggplot(dat, aes(f, bark)) +
  geom_line()

f_tones <- ggplot(dat, aes(f, tones)) +
  geom_line()

tones_bark <- ggplot(dat, aes(tones, bark)) +
  geom_line()

cowplot::plot_grid(f_bark,
                   f_tones,
                   tones_bark)
