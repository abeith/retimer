# Alter F0 contour

# Function for calculating pitch by intervals from refrence
# tones sets the scale: i.e. tones = 12 is 12 tone equal tempered (12-TET)
noteFreq <- function(ref, intervals, tones = 12) {
  ratio <- 2^(1/tones)
  ref * ratio^intervals}

# Convert frequency to tones and tones to frequency: specify tones in scale (i.e. 12-TET; 24-TET)
# Equations adapted from http://www.sengpielaudio.com/calculator-centsratio.htm
freqToTones <-  function(x, ref) {12*(log(x/ref)/log(2))}
tonesToFreq <- function(intervals, ref, tones){2^(intervals/tones)*ref}

# Resynthesise wav file with flat F0, .f can be any central tendency function
flatF0 <- function(fileName, .f = findPeak){
  # Extract Pitch Tier
  praatSys(paste(fileName, "audio", "/"))

  # Read Pitch Tier
  pitchTier <- pt.read(paste0("outputs/", fileName, ".PitchTier"))

  # Create new pitch tier
  newPitchTier <- pitchTier
  newPitchTier$t <- c(min(pitchTier$t), max(max(pitchTier$t)))
  newPitchTier$f <- c(rep(.f(pitchTier$f), 2))

  funName <- as.character(substitute(.f))

  # Write PT
  pt.write(newPitchTier, paste0("outputs/", fileName, "_", funName, ".PitchTier"))

  # Resynthesise
  praatSys(paste(fileName, funName, "/"), "reSynthPitch.praat")
}

#Resynthesise wav file with multiplied F0 tone range: Default halves range
#shift and autotune are fun for music: Not so useful for speech
convertAudio <- function(fileName, factor = 0.5, shift = 0, autotune = FALSE){
  # Extract Pitch Tier
  praatSys(paste(fileName, "audio", "/"))

  # Read Pitch Tier
  pitchTier <- pt.read(paste0("outputs/", fileName, ".PitchTier"))

  pt.plot(pitchTier)

  if(autotune){
    peakPitch <- median(pitchTier$f) %>%
      freqToTones(440) %>%
      round %>%
      tonesToFreq(440, 12)
  }else{
    peakPitch <- findPeak(pitchTier$f)
  }

  # Plot density of pitch with vline on chosen peak
  plot(density(pitchTier$f))
  abline(v = peakPitch)

  # Create new pitch tier
  newPitchTier <- pitchTier

  fInTones <- pitchTier$f %>%
    map_dbl(~freqToTones(.x, peakPitch))

  tonesInF <- fInTones %>%
    map_dbl(~tonesToFreq(.x, peakPitch, 12 / abs(factor)))

  tonesInF <- tonesInF * sign(factor)

  if(autotune){
    # Filter out flats and sharps
    newPitchTier$t <- newPitchTier$t[abs(fInTones - round(fInTones)) < 0.25]
    newPitchTier$f <- tonesInF
    newPitchTier$f <- newPitchTier$f[abs(fInTones - round(fInTones)) < 0.25]
  }else {
    newPitchTier$f <- tonesInF
  }

  pt.plot(newPitchTier)

  # Write PT
  pt.write(newPitchTier, paste0("outputs/", fileName, "_", factor, ".PitchTier"))

  # Resynthesise
  praatSys(paste(fileName, factor, "/"), "reSynthPitch.praat")
}

# Load all pitch tiers into a tibble for comparison. fileName is passed as pattern
pt.compare <- function(path, fileName, extension = ".PitchTier", ...){
  files <- list.files(path = path, pattern = paste0(fileName, ".*", extension), ...)
  dat <- tibble(files = files) %>%
    mutate(pitchTiers = map(paste0(path, "/", files), pt.read),
           pitchTiers = map(pitchTiers, as_tibble),
           pitchTiers = map2(pitchTiers, files, ~mutate(.x, id = .y))) %>%
    pull(pitchTiers) %>%
    bind_rows() %>%
    mutate(id = str_remove(id, extension))
  return(dat)
}

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
