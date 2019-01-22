# Function for finding a word in TextGrids
findWord <- function(x, word = "speech"){

  sprintf("./grids/%s.TextGrid", x) %>%
    loadGrid() %>%
    pluck("Word") %>%
    filter(label == word) %>%
    mutate(x = x)
}

# Function for extracting wav of word
extractWord <- function(name, newName= "extracted", start = 10, end = 20, script = "/scripts/extractWord.praat", praatPath = "C:\\Program Files\\praat6043_win64\\Praat.exe", args = "--run"){

    # Quote path if using Windows
  if(.Platform$OS.type == "windows"){
    praatPath <- paste0("\"", praatPath, "\"")}else{
      praatPath <- "praat"
    }

  scriptPath <- paste0("\"", getwd(), script, "\"")

  system(paste(praatPath, args, scriptPath, name, newName, start, end))

  }
