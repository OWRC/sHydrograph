

# hydrograph info
loc.info <- function(ndat,DTb,DTe,stat,nam){
  por <- as.integer(difftime(DTe, DTb, units = "days"))
  a <- cbind(nam,round(stat,1))
  tb <- paste0('<tr><td>',a[,1],'&nbsp;&nbsp;</td><td align="right">', a[,2],'</td></tr>', collapse = "")
  
  paste0(
    strftime(DTb, "%b %Y"),' to ',strftime(DTe, "%b %Y"),' (',por+1,' days)</div>',
    # '<div>total missing: ',por-ndat,' days (',round((1-ndat/por)*100,0),'%)</div>', br(),
    '<div>total data days: ',ndat,' (',round((ndat/por)*100,0),'%)</div>',
    
    '<div><h4>mean values:</h4></div>',
    '<table>',tb,'</table>'
  )
}