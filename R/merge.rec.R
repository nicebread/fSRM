merge.rec <-
function(.list, ...){
	if(length(.list)==1) return(.list[[1]])
	Recall(c(list(merge(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
}
