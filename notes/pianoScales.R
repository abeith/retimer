library(retimer)

# 12 tone scale: Useful for plotting contours
pianoScale <- function(){
  tibble(notes = c(letters[1:7], letters[c(1, 3:4, 6:7)] %>%
                     paste0("#")) %>%
           sort %>%
           .[c(4:12, 1:3)] %>%
           paste0(rep(1:7, each = 12))) %>%
    mutate(tones = row_number() - row_number()[notes == "a4"],
           freq = tonesToFreq(tones, 440, 12))
}

pianoBreaks <- function(){pianoScale %>%
    filter(!str_detect(notes, "#"))}


notes <- sort(c(letters[1:7], paste0(letters[c(1, 3:4, 6:7)], "#")))

scales::log_trans

library(tidyverse)

x <- 1:100
y <- sample(tonesToFreq(-36:36, 440, 12), 100, replace = TRUE)
df <- tibble(x = x, y = y)


musical <- function(ref = 440){

  breaks <- tonesToFreq(-36:36, ref, 12)
  trans <- function(x) freqToTones(x, ref)
  inv <- function(x) tonesToFreq(x, ref, 12)
  scales::trans_new("musical", trans, inv)
}

note_names <- paste0(c(notes[4:12], notes[1:3]), rep(1:8, each = 12))
note_freqs <- tonesToFreq(-45:50, 440, 12)

note_freqs[which(note_names == "a4")]

ggplot(df, aes(x, y)) +
  geom_line() +
  scale_y_continuous(trans = musical(),
                     breaks = note_freqs[which(!grepl("#", note_names))],
                     labels = note_names[which(!grepl("#", note_names))])
