#' wsola
#'
#' Waveform Similarity Overlap-add. Translated from TSM Toolbox.
#'
#' @param x an audio signal
#' @param s a scaling factor or a list of two vector with anchor points
#' @param win window function. Default is 'hann' for hanning window. Can also be a custom window supplied as a vector
#' @param synHop synthesis window hop size
#' @param tol tolerance for overlap delta
#'
#' @return retimed audio signal as vector
#'
#' @references Driedger, J., Müller, M., Fourier, J., & Shannon, C. (2014). TSM Toolbox: MATLAB Implementations of Time-Scale Modification Algorithms. 8.
#' @seealso fft_spectrum
#' @export

wsola <- function(x, s, win = 'hann', winLen = 1024, synHop = 512, tol = 512){

  if(is.numeric(s) && length(s) == 1){
    anc_in <- c(1, length(x))
    anc_out <- c(1, ceiling(length(x) * s))
  }else if(is.list(s) && length(s)){
    anc_in <- s[[1]]
    anc_out <- s[[2]]
  }else{
    stop('s should be with a factor or a list of 2 vectors')
  }

  winLenHalf <- round(winLen/2)

  if(win == 'hann'){
    w <- signal::hanning(winLen)
  }else if(length(win) == winLen){
    w <- win
  }else{
    stop('win can be \'hann\' for a hanning window or a vector with length of winLen for custom windows')
  }

  ## Time stretch function
  outputLength <- anc_out[length(anc_out)]
  synWinPos <- seq(1, outputLength + winLenHalf, synHop)
  anaWinPos <- round(signal::interp1(anc_out, anc_in, synWinPos, extrap = T))
  anaHop <- c(0, anaWinPos[2:length(anaWinPos)] - anaWinPos[1:(length(anaWinPos) - 1)])

  ## wsola
  y <- rep(0, outputLength)
  minFac <- min(synHop / anaHop)
  xC <- c(rep(0, winLenHalf + tol), x, rep(0, ceiling(1/minFac) * winLen + tol))
  anaWinPos <- anaWinPos + tol
  yC <- rep(0, outputLength + 2 * winLen)
  ow <- yC
  del <- 0

  ## Loop over analysis window frames
  for(i in 1:(length(anaWinPos) - 1)){
    currSynWinRan <- synWinPos[i]:(synWinPos[i] + winLen - 1)
    currAnaWinRan <- (anaWinPos[i] + del):(anaWinPos[i] + winLen - 1 + del)
    yC[currSynWinRan] <- yC[currSynWinRan] + xC[currAnaWinRan] * w
    ow[currSynWinRan] <- ow[currSynWinRan] + w
    natProg <- xC[currAnaWinRan + synHop]
    nextAnaWinRan <- (anaWinPos[i+1] - tol):(anaWinPos[i+1] + winLen - 1 + tol)
    xNextAnaWinRan <- xC[nextAnaWinRan]
    cc <- crossCorr(xNextAnaWinRan, natProg, winLen)
    maxIndex <- which.max(cc)
    del <- tol - maxIndex + 1
  }

  ## last frame
  yC[synWinPos[length(synWinPos)]:(synWinPos[length(synWinPos)] + winLen - 1)] <- yC[synWinPos[length(synWinPos)]:(synWinPos[length(synWinPos)] + winLen - 1)] + xC[(anaWinPos[i] + del):(anaWinPos[i] + winLen - 1 + del)] * w
  ow[synWinPos[length(synWinPos)]:(synWinPos[length(synWinPos)] + winLen - 1)] <- ow[synWinPos[length(synWinPos)]:(synWinPos[length(synWinPos)] + winLen - 1)] + w

  ## Normalise signal
  ow[ow < 10e-3] <- 1
  yC <- yC / ow

  ## Remove padding
  yC <- yC[(winLenHalf + 1):length(yC)]
  yC <- yC[1:outputLength]

  return(yC)
}

crossCorr <- function(x, y, winLen){
  cc <- signal::conv(rev(x), y)
  cc <- cc[winLen:(length(cc) - winLen + 1)]
  return(cc)
}
