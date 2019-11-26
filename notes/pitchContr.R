# Alter F0 contour

# Function for calculating pitch by intervals from refrence
# tones sets the scale: i.e. tones = 12 is 12 tone equal tempered (12-TET)
noteFreq <- function(ref, intervals, tones = 12) {
  ratio <- 2^(1/tones)
  ref * ratio^intervals}

# Convert frequency to tones and tones to frequency: specify tones in scale (i.e. 12-TET; 24-TET)
freqToTones <-  function(x, ref) {12*(log(x/ref)/log(2))}
tonesToFreq <- function(intervals, ref, tones){2^(intervals/tones)*ref}

#Resynthesise wav file with multiplied F0 tone range: Default halves range
#shift and autotune are fun for music: Not so useful for speech
convertAudio <- function(fileName, factor = 0.5, shift = 0, resolution = 0.1, min = 75, max = 500, stylize = 1, autotune = FALSE){
  # Extract Pitch Tier
  #praatSys(paste(fileName, "audio", "/", resolution, min, max, stylize))

  # Read Pitch Tier
  pitchTier <- rPraat::pt.read(paste0("outputs/", fileName, ".PitchTier"))

  rPraat::pt.plot(pitchTier)

  if(autotune){
    peakPitch <- stats::median(pitchTier$f) %>%
      freqToTones(440) %>%
      round %>%
      tonesToFreq(440, 12)
  }else{
    peakPitch <- findPeak(pitchTier$f)
  }

  # Plot density of pitch with vline on chosen peak
  graphics::plot(stats::density(pitchTier$f))
  graphics::abline(v = peakPitch)

  # Create new pitch tier
  newPitchTier <- pitchTier

  fInTones <- pitchTier$f %>%
    purrr::map_dbl(~freqToTones(.x, peakPitch))

  fInTones <- (fInTones + shift) * sign(factor)

  tonesInF <- fInTones %>%
    purrr::map_dbl(~tonesToFreq(.x, peakPitch, 12 / abs(factor)))

  if(autotune){
    # Filter out flats and sharps
    newPitchTier$t <- newPitchTier$t[abs(fInTones - round(fInTones)) < 0.25]
    newPitchTier$f <- tonesInF
    newPitchTier$f <- newPitchTier$f[abs(fInTones - round(fInTones)) < 0.25]
  }else {
    newPitchTier$f <- tonesInF
  }

  rPraat::pt.plot(newPitchTier)

  outFile <- paste0(fileName, "_t", factor, "_p", shift)

  # Write PT
  rPraat::pt.write(newPitchTier, paste0("outputs/", outFile, ".PitchTier"))

  # Resynthesise
  praatSys(paste(fileName, factor, shift, "/", resolution, min, max), "reSynthPitch.praat")
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

