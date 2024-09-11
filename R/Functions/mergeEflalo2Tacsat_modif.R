mergeEflalo2Tacsat_modif <- function (eflalo2, tacsat) 
{
  if (!"D_DATIM" %in% colnames(eflalo2)) 
    eflalo2$D_DATIM <- as.POSIXct(paste(eflalo2$FT_DDAT, 
                                        eflalo2$FT_DTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (!"L_DATIM" %in% colnames(eflalo2)) 
    eflalo2$L_DATIM <- as.POSIXct(paste(eflalo2$FT_LDAT, 
                                        eflalo2$FT_LTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (!"SI_DATIM" %in% colnames(tacsat)) 
    tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, 
                                        tacsat$SI_TIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
  if (class(eflalo2$VE_REF) != "character") 
    eflalo2$VE_REF <- ac(eflalo2$VE_REF)
  if (class(tacsat$VE_REF) != "character") 
    tacsat$VE_REF <- ac(tacsat$VE_REF)
  Ef <- orderBy(~VE_REF + D_DATIM + L_DATIM, data = eflalo2)
  Ta <- orderBy(~VE_REF + SI_DATIM, data = tacsat)
  splitEf <- split(Ef, Ef$VE_REF)
  splitTa <- split(Ta, Ta$VE_REF)
  tacefmatch <- pmatch(sort(unique(Ta$VE_REF)), sort(unique(Ef$VE_REF)))
  for (i in 1:length(tacefmatch)) {
    pm <- tacefmatch[i]
    eftim <- splitEf[[pm]][which(duplicated(splitEf[[pm]]$FT_REF) == 
                                   FALSE), c("D_DATIM", "L_DATIM", "FT_REF")]
    # dtime <- eftim[, 1]
    dtime <- eftim$D_DATIM
    # ltime <- eftim[, 2]
    ltime <- eftim$L_DATIM
    stime <- splitTa[[i]]$SI_DATIM
    # tripn <- eftim[, 3]
    tripn <- eftim$FT_REF
    if (is.na(tacefmatch[i]) == TRUE) {
      splitTa[[i]]$FT_REF <- 0
    }
    else {
      smdtime <- t(outer(stime, dtime, "-"))
      gtltime <- outer(ltime, stime, "-")
      st <- apply(smdtime, 1, function(x) {
        which(x >= 0)[1]
      })
      en <- apply(gtltime, 1, function(x) {
        rev(which(x >= 0))[1]
      })
      subse <- which(is.na(st <= en) == FALSE & (st <= 
                                                   en) == TRUE)
      st <- st[subse]
      en <- en[subse]
      if (length(st) != 1) {
        idx <- unlist(mapply(seq, st, en, SIMPLIFY = FALSE))
        reps <- unlist(lapply(mapply(seq, st, en, SIMPLIFY = FALSE), 
                              length))
        splitTa[[i]]$FT_REF <- 0
        splitTa[[i]]$FT_REF[idx] <- rep(tripn[subse], 
                                        reps)
      }
      if (length(st) == 1) {
        splitTa[[i]]$FT_REF <- 0
        splitTa[[i]]$FT_REF[seq(st, en)] <- rep(tripn[subse], 
                                                length(seq(st, en)))
      }
      if (length(st) == 0) {
        splitTa[[i]]$FT_REF <- 0
      }
    }
  }
  Ta$FT_REF <- unlist(lapply(splitTa, function(x) {
    return(x$FT_REF)
  }))
  return(Ta)
}
