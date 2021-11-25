

remove.outliers <- function(df) {
  # outlier removal
  med <- median(df$Val)
  if ( med==0 & max(df$Val,na.rm=TRUE)>0 ) {
    med <- median(df$Val[df$Val!=0]) 
    iqr <- IQR(df$Val[df$Val!=0])*100
    df$Val[df$Val<(med-iqr) & df$Val!=0]=NA
    df$Val[df$Val>(med+iqr) & df$Val!=0]=NA    
  } else {
    iqr <- IQR(df$Val)*100
    df$Val[df$Val<(med-iqr)]=NA
    df$Val[df$Val>(med+iqr)]=NA    
  }
  return(df[c('Date','Val')])
}