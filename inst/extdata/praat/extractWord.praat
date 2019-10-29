form Specify file
    text path .
    text newName new
    real start
    real end
    text wd
endform

inPath$ = wd$ + "/" + path$ + ".wav"
outPath$ = wd$ + "/" + path$ + "_" + newName$ + ".wav"

sound = Read from file: inPath$

select sound
outFile = Extract part... start end "rectangular" 1.0 "yes"
select outFile
Save as WAV file: outPath$
