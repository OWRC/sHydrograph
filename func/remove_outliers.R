

remove.outliers <- function(df,col) {
  df1 <- df[which(df$RDNC == col),]
  # outlier removal
  med <- median(df1$Val)
  iqr <- IQR(df1$Val)*100
  df1$Val[df1$Val<(med-iqr)]=NA
  df1$Val[df1$Val>(med+iqr)]=NA
  return(df1[c('Date','Val')])
}