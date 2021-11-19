

remove.outliers <- function(df) {
  # outlier removal
  med <- median(df$Val)
  iqr <- IQR(df$Val)*100
  df$Val[df$Val<(med-iqr)]=NA
  df$Val[df$Val>(med+iqr)]=NA
  return(df[c('Date','Val')])
}