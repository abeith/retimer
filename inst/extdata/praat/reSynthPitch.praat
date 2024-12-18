# Specify variables
form Specify file
    text name
    text pt_new
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + "/" + name$ + ".wav"
manipulation = Read from file: wd$ + "/" + name$ + ".Manipulation"
pitchTier = Read from file: wd$ + "/" + pt_new$

# Resynthesise speech
selectObject: manipulation
plusObject: pitchTier
Replace pitch tier
selectObject: manipulation
outSound = Get resynthesis (overlap-add)

# Save output
selectObject: outSound
Save as WAV file: wd$ + "/" + name$ + "_resynth" + ".wav"
