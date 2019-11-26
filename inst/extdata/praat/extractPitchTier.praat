# Specify variables (defaults only used if executed from Praat)
form Specify file
    text inFile A01
    positive resolution 0.1
    positive min 50
    positive max 250
    boolean stylize 1
    text wd
endform

# Load file and get reference values
inSound = Read from file: wd$ + "/" + inFile$ + ".wav"
manipulation = To Manipulation: resolution, min, max
pitchTier = Extract pitch tier
# pitchTier = Stylize: 2, "Semitones"
if stylize = 1
  pitchTier = Interpolate quadratically: 4, "Semitones"
endif

# Add points to start and end of pitch tier
minT = Get time from index: 1
minTF = Get value at time: minT
Add point: 0, minTF

maxP = Get number of points
maxT = Get time from index: maxP
maxTF = Get value at time: maxT
endT = Get end time
Add point: endT, maxTF

# Save pitch tier
selectObject: pitchTier
Save as PitchTier spreadsheet file: wd$ + "/" + inFile$ + ".PitchTier"

# Save manipulation
selectObject: manipulation
Save as text file: wd$ + "/" + inFile$ + ".Manipulation"

