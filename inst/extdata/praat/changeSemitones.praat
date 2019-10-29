# Specify variables (defaults only used if executed from Praat)
form Specify file
    text inFile D03_maths_extract
    text factor 0.5
    text shift 0
    text slash \
    positive resolution 0.1
    positive min 50
    positive max 250
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + slash$ + "audio" + slash$ + inFile$ + ".wav"
# manipulation = To Manipulation: resolution, min, max
manipulation = Read from file: wd$ + slash$ + "outputs" + slash$ + inFile$ + ".Manipulation"
pitchTier = Read from file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_t" + factor$ + "_p" + shift$ + ".PitchTier"

# Resynthesise speech
selectObject: manipulation
plusObject: pitchTier
Replace pitch tier
selectObject: manipulation
outSound = Get resynthesis (overlap-add)

# Save output
selectObject: outSound
Save as WAV file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_t" + factor$ + "_p" + shift$ + ".wav"
