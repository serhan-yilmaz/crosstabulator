
table1 = function(curdf, colfactor, selectedFields, colOptions="", add_total_col=T, statistics=T, NEJMstyle=T, digits=2, colN=T, 
  caption="", pos.caption="top", tfoot="",  continuous_fns="describeMean", css.class="gmisc_table", hrzl_prop=T) {
  
  
  if (is.vector(selectedFields)) {
    selectedFields = cbind(selectedFields, rep(digits, times=length(selectedFields)))
  }
  
  if (colOptions == "") {
    x = levels(curdf[,colfactor])
    colOptions = cbind(x, rep("c", length(x)), rep(label(curdf[,colfactor]), length(x)))
  }
  
  #continuous_fns = c(rep("describeMedian", 1), rep("describeMean", nrow(selectedFields)-1))
  #continuous_fns = rep("describeMedian", 1)
  if(length(continuous_fns)){
    continuous_fns = rep(continuous_fns, nrow(selectedFields))
  }
  
  #message("Table1: Status 0")
  # Get the basic stats and store in a list
  table_data <- list()
  for (i in 1:nrow(selectedFields)) {
    cnt_fn = switch (continuous_fns[i],
            "describeMedian" = describeMedian,
            "describeMean" = describeMean,
            stop("Invalid continuous function selected.")
    )
    table_data[[ selectedFields[i,1] ]] = 
      getDescriptionStatsBy(curdf[, selectedFields[i,1]], curdf[, colfactor], show_all_values=TRUE, hrzl_prop = hrzl_prop, html=TRUE, continuous_fn = cnt_fn, 
        add_total_col=add_total_col, statistics=statistics, NEJMstyle = NEJMstyle, digits=as.integer(selectedFields[i,2]))
  }
  
  #message("Table1: Status A")
  
  # Now merge everything into a matrix
  # and create the rgroup & n.rgroup variabels
  rgroup <- c()
  n.rgroup <- c()
  output_data <- NULL
  for (varlabel in names(table_data)){
    output_data <- rbind(output_data, table_data[[varlabel]])
    rgroup <- c(rgroup, varlabel)
    n.rgroup <- c(n.rgroup, nrow(table_data[[varlabel]]))
  }
  
  #message("Table1: Status B")
  
  # build N= col headings
  if (colN) 
    headings = sapply(colnames(output_data), function(x) {
      if (x=="Total")
        paste0(x, " (n=", nrow(curdf), ")")
      else if (x=="P-value")
        paste0(x)
      else
        paste0(x, " (n=", sum(!is.na(curdf[, colfactor]) & (curdf[, colfactor]==x)), ") ")
    })
  else
    headings = colnames(output_data)
  
  #message("Table1: Status C")
  
  # build cgroup from colOptions
  cgroup=c("")
  n.cgroup=c(0)
  if (add_total_col) n.cgroup=c(1)
  
  for (i in 1:nrow(colOptions)) {
    if (colOptions[i,3] == cgroup[length(cgroup)]){ # if curgroup same as the last one
      n.cgroup[length(n.cgroup)] = n.cgroup[length(n.cgroup)]+1
    }
    else {
      n.cgroup = c(n.cgroup, 1)
      cgroup = c(cgroup, colOptions[i,3])
    }
  }
  if (statistics) { 
    n.cgroup = c(n.cgroup,1) 
    cgroup = c(cgroup, "")
  }
  
  #message("Table1: Status D")
  
  # new version of GMisc uses missing instead of null for empty options
  #if (all(cgroup==c(""))) cgroup=NULL
  
  # build column alignment from colOptions
  if (add_total_col) align="c" else align=""
  for (i in 1:nrow(colOptions))
    align = paste0(align, colOptions[i,2])
  if (statistics) align=paste0(align, "c")
  
  #message("Table1: Status E")
  
  x = htmlTable(output_data, align=align,
    
    rgroup=rgroup, 
    n.rgroup=n.rgroup, 
    
    cgroup = cgroup,
    n.cgroup = n.cgroup,
    
    header=headings,
    rowlabel="", 
    
    caption=caption, 
    pos.caption = pos.caption,
    
    tfoot=tfoot, 
    ctable=TRUE, 
    
    css.class = css.class
    , css.table = "width: 100%;"
    )
  
  #message("Table1: Status F")
    
  #addHtmlTableStyle(css.table = "display: block")
  
  out_list <- list("html" = x, "table" = output_data, "rgroup" = rgroup, "n.rgroup" = n.rgroup, "headings" = headings)
  
  #message("Table1: Status X.")
    
  return(out_list)
}
