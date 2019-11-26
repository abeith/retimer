form Specify file
    text audio
    text grid
    text outfile
    text wd
endform

inPath$ = wd$ + "/" + audio$
inGrid$ = wd$ + "/" + grid$
outPath$ = wd$ + "/" + outfile$

sound = Read from file: inPath$
grid = Read from file: inGrid$

selectObject: sound
manipulation = To Manipulation: 0.01, 75, 600
durationTier = Extract duration tier

selectObject: grid
n = Get number of intervals: 1

for i to n
  selectObject: grid
  startOld = Get start time of interval: 1, i
  endOld = Get end time of interval: 1, i
  startNew = Get start time of interval: 2, i
  endNew = Get end time of interval: 2, i
  durationOld = endOld - startOld
  durationNew = endNew - startNew
  factor = durationNew / durationOld
  selectObject: durationTier
  Add point: startOld + 0.01, factor
  Add point: endOld - 0.01, factor
endfor

selectObject: manipulation
plusObject: durationTier
Replace duration tier

selectObject: manipulation
outSound = Get resynthesis (overlap-add)

selectObject: outSound
Save as WAV file: outPath$
