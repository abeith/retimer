# Specify variables (defaults only used if executed from Praat)
form Specify file
    text inFile D03_maths_extract
    text newName squeezed
    positive factor 0.5
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + "audio\" + inFile$ + ".wav"
manipulation = To Manipulation: 0.01, 75, 600
pitchTier = Extract pitch tier
pitchObj = To Pitch: 0.02, 60, 400
minPitch = Get minimum: 0, 0, "Hertz", "Parabolic"
maxPitch = Get maximum: 0, 0, "Hertz", "Parabolic"

# Multiply pitches by factor and then adjust minimum
# frequency to return to normal minimum F0
selectObject: pitchTier
Multiply frequencies: 0, 1000, factor
Shift frequencies: 0, 1000, minPitch * factor, "Hertz"

# Resynthesise speech
selectObject: manipulation
plusObject: pitchTier
Replace pitch tier
selectObject: manipulation
outSound = Get resynthesis (overlap-add)

# Save output
selectObject: outSound
Save as WAV file: wd$ + "outputs\" + inFile$ + newName$ + ".wav"

