form Specify file
    text inFolder audio
    text wd
endform

Create Strings as file list: "wavList", wd$ + "/" + "audio" + "/*.wav"
numberOfStrings = Get number of strings
for stringCounter from 1 to numberOfStrings
  select Strings wavList
  filename$ = Get string ... 'stringCounter '
  Read from file: wd$ + "/" + inFolder$ + "/" + "filename$"
endfor
