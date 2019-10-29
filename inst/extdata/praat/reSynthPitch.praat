# Specify variables (defaults only used if executed from Praat)
form Specify file
    text name A01
    text pt mean
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + "/" + name$ + ".wav"
manipulation = Read from file: wd$ + "/" + name$ + ".Manipulation"
pitchTier = Read from file: wd$ + "/" + name$ + "_" + pt$ + ".PitchTier"

# Resynthesise speech
selectObject: manipulation
plusObject: pitchTier
Replace pitch tier
selectObject: manipulation
outSound = Get resynthesis (overlap-add)

# Save output
selectObject: outSound
Save as WAV file: wd$ + "/" + name$ + "_" + pt$ + ".wav"
