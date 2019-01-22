form Specify file
    text name
    text newName
    real start
    real end
endform

wd$ = "..\"
inPath$ = wd$ + "audio\" + name$ + ".wav"
outPath$ = wd$ + "outputs\" + name$ + "_" + newName$ + ".wav"

sound = Read from file: inPath$

select sound
outFile = Extract part... start end "rectangular" 1.0 "yes"
select outFile
Save as WAV file: outPath$