prettyunits <- function(temp00, combine=T) {
  
  temp00<-sum(as.numeric(temp00))
  if (is.na(temp00)) {
    out<-NA
  } else {
    
    sigfig<-format(temp00, digits = 3, scientific = TRUE)
    sigfig0<-as.numeric(substr(x = sigfig, start = (nchar(sigfig)-1), stop = nchar(sigfig)))
    
    if (sigfig0<=5) {
      # if (sigfig0<4) {
      unit<-""
      x<-format(x = temp00, big.mark = ",", digits = 0, scientific = F)
       } else if (sigfig0>=4 & sigfig0<6) {
         unit<-" thousand"
       x<-round(temp00/1e3, digits = 1)
       } else if (sigfig0==5) {
         unit<-" thousand"
         x<-round(temp00/1e3, digits = 0)
    } else if (sigfig0>=6 & sigfig0<9) {
      unit<-" million"
      x<-round(temp00/1e6, digits = 1)
    } else if (sigfig0>=9 & sigfig0<12) {
      unit<-" billion"
      x<-round(temp00/1e9, digits = 1)
    } else if (sigfig0>=12) {
      unit<-" trillion"
      x<-round(temp00/1e12, digits = 1)
    }
    
    out<-ifelse(combine==T, paste0(x, unit), list(x, unit))
  }
  
  return(out)
}