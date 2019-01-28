# Specify variables (defaults only used if executed from Praat)
form Specify file
    text inFile D03_maths_extract
    text inFolder audio
    text slash \
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + slash$ + inFolder$ + slash$ + inFile$ + ".wav"
manipulation = To Manipulation: 0.01, 75, 600
pitchTier = Extract pitch tier

# Save pitch tier
selectObject: pitchTier
Save as PitchTier spreadsheet file: wd$ + slash$ + "outputs" + slash$ + inFile$ + ".PitchTier"
