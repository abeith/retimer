form Specify file
    text inPath .
    text outPath .
    real start
    real end
    text wd
endform

sound = Read from file: inPath$

select sound
outFile = Extract part... start end "rectangular" 1.0 "yes"
select outFile
Save as WAV file: outPath$
