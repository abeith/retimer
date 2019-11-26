form Specify file
    text inFile fileName
    positive semitones 12
    text slash \
    text wd
endform

inSound = Read from file: wd$ + slash$ + "audio" + slash$ + inFile$ + ".wav"
runScript: "C:\Users\alistaib\Praat\plugin_VocalToolkit\changesemitones.praat", semitones
Save as WAV file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_" + string$(semitones) + ".wav"
