##' get_serial_anchors
##'
##' Convert a set of point anchors to a set of anchors that prevent overlaps while fixing the retiming factor within words.
##' 
##' @param anc_in a vector of time points in the input signal
##' @param anc_out a vector of the times anc_in should be mapped to in the output signal
##' @param w_onsets a vector of time points for the onsets of words. Should be same length as anc_in.
##' @param w_offsets a vector of time points for the offsets of words. Should be same length as anc_in.
##' @param fs Sample rate of signal. If provided, returned anchor points with be expressed in samples. If NULL result will be expressed in seconds.
##' @param retime_f The desired factor that words should be sped by. If NULL the minimum change in rate that will prevent overlaps will be calculated.
##' @param dry_run If TRUE function will exit early with the minimum factor that will prevent overlaps.
##' @param smudge If > 0 this applies a crude adjustment to the calculated anchors to ensure monotonicity. Not necessary unless w_onsets are same as previous w_offsets.
##' @return A list that can be used to perform retiming with the wsola function of this package.
##' @seealso wsola
##' @export

get_serial_anchors <- function(anc_in, anc_out, w_onsets, w_offsets, fs = NULL, retime_f = NULL, dry_run = FALSE, smudge = 0){
    ## Check if retiming is possible
    intervals_in <- anc_in[2:length(anc_in)] - anc_in[1:(length(anc_in)-1)]
    intervals_out <- anc_out[2:length(anc_out)] - anc_out[1:(length(anc_out)-1)]
    max_factor <- min(intervals_out/intervals_in)

    ## Can be used to figure out maximum possible retime factor that prevents overlap
    if(dry_run){
      return(max_factor)
    }

    if(is.null(retime_f)){
      retime_f <- max_factor
    }

    ## When not performing dry run exit if the retime factor is set too high
    if(retime_f > max_factor){
      stop(sprintf('retime factor must be %0.2f or less to avoid overlaps', max_factor))
    }

    head_lens <- anc_in - w_onsets
    tail_lens <- w_offsets - anc_in

    w_onsets_out <- anc_out - (retime_f * head_lens)
    w_offsets_out <- anc_out + (retime_f * tail_lens)

    new_anc_in <- as.vector(rbind(w_onsets, w_offsets))
    new_anc_out <- as.vector(rbind(w_onsets_out, w_offsets_out))

    ## Return anchors in original units if fs not set
    ## Won't be usable with wsola function
    ## If fs is defined add initial and final values and scale to samples
    if(is.null(fs)){
      return(list(anc_in = new_anc_in, anc_out = new_anc_out))
    }
    else{
      anc_m <- cbind(new_anc_in, new_anc_out)
      ## Fix cases where rate is increased
      anc_m[,2] <- anc_m[,2] - min(anc_m[,2])
      anc_m <- unique(anc_m) * fs

      ## Stretch static points in source to avoid artifacts
      if(smudge){
        message('smudging')
        sil_i <- which(anc_m[2:length(new_anc_in),1] == anc_m[1:(length(new_anc_in)-1),1])
        ## Allow 100ms of smudging
        smudge <- smudge*(fs/1000)
        anc_m[sil_i,1] <- anc_m[sil_i,1] - smudge
        ## anc_m[sil_i+1,1] <- anc_m[sil_i+1,1] + smudge
      }

      return(list(anc_in = anc_m[,1], anc_out = anc_m[,2]))
    }
  }
