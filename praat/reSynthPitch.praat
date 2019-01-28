# Specify variables (defaults only used if executed from Praat)
form Specify file
    text inFile D03_maths_extract
    text factor 0.5
    text slash \
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + slash$ + "audio" + slash$ + inFile$ + ".wav"
manipulation = To Manipulation: 0.01, 75, 600
pitchTier = Read from file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_" + factor$ + ".PitchTier"

# Resynthesise speech
selectObject: manipulation
plusObject: pitchTier
Replace pitch tier
selectObject: manipulation
outSound = Get resynthesis (overlap-add)

# Save output
selectObject: outSound
Save as WAV file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_" + factor$ + ".wav"
