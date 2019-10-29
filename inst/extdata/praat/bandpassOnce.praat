form Specify file
    text inFile D03_maths_extract
		positive min 100
		positive max 10000
    text band 1
    text slash
    text wd
endform

sound = Read from file: wd$ + slash$ + "audio" + slash$ + inFile$ + ".wav"
selectObject: sound
outFile = Filter (pass Hann band): min, max, min/5
Save as WAV file: wd$ + slash$ + "outputs" + slash$ + inFile$ + "_bandpass_" + string$(min) + "_" + string$(max) + ".wav"
