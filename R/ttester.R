#' Read Mmultiple Student t-Tests Function
#'
#' This function performs multiple t tests between all pairs of columns/lists of a data file and returns a readable output
#' @param data The data file that contains the groups that you want to compare as columns or lists
#' @param pairedvalue FALSE indicates that the stat::t.test performed will be unpaired, TRUE will be paired (default is FALSE)
#' @param varvalue FALSE indicates that equal variance should not be assumed, TRUE assumes equal variance #note in order for the varvalue param to have effect, pairedvalue must be set to FALSE - this choice depends on your study design
#' @param pval_sci TRUE if desired output is scientific notation
#' @return A list of t.test results and tested_pairs 
#' @examples
#' ttester(testdata = data.frame(VADeaths), pairedvalue = FALSE, pval_sci = TRUE)
#' @export
### used to also include this... where the poundsign was substituted for an @ to create an object with the output in the above section



ttester<- function(testdata, pairedvalue=FALSE, varvalue=FALSE, pval_sci = FALSE) {
  if(length(names(testdata))<2){print("ttester requires a matrix or a data frame with multiple columns or list of lists to compare")
    }else {
  test_names <<- data.frame(combn(names(testdata), m= 2))
  tresults<<-list()
  ttestStats<-data.frame()
  tested_names1<<-list()
  tested_names2<<-list()
  for(i in names(test_names)){
    Group1_data <-testdata[which(names(testdata) %in% test_names[i][1,])]
    Group2_data <-testdata[which(names(testdata) %in% test_names[i][2,])]
    tested_names1[[i]]<-names(testdata)[names(testdata) %in% test_names[i][1,]]
    tested_names2[[i]]<-names(testdata)[names(testdata) %in% test_names[i][2,]]
    tresults[[i]]<<-t.test(x=Group1_data[[1]], y=Group2_data[[1]], paired = pairedvalue, var.equal = varvalue)
    tested_pairs <<-paste0(tested_names1, "&", tested_names2)
    names(tresults)<<-tested_pairs
  }
  get_pretty<-function(tresults, tested_pairs, pval_sci) {
    c(t =format(tresults$statistic, digits = 2),
    df = format(tresults$parameter, digits = 0),
    p.value = format(tresults$p.value, scientific = pval_sci, digits = 2),
    bonferroni.threshold = format(.05/length(tested_pairs), digits = 3),
    sig = ifelse(tresults$p.value<(.05/length(tested_pairs)), "yes", "no"),
    greater_mean = ifelse(tresults$statistic >0, "First group greater mean", "Second group greater mean"))
  }
  ttestStats <<-data.frame(lapply(tresults, get_pretty, pval_sci = pval_sci, tested_pairs=tested_pairs))
    }
ttestStats
}


