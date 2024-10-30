formulator <- function(text, words=4, write.file=NULL, sub = NULL, sub.with = NULL){

# otionally substitute some parts of the text. "et al." 2 "etal" is standard
if(is.null(sub)) sub <- character()
if(is.null(sub.with)) sub.with<- character()
sub <- c(sub, "et al.", "i\\.e\\.", "e\\.g\\.")
sub.with <- c(sub.with, "etal", "i.e", "e.g")
if(length(sub)==length(sub.with) | length(sub.with)==1){
	for( i in 1:length(sub)) text <- gsub(sub[i], sub.with[ifelse(length(sub.with)==1,1,i)], text)
} else print("sub and sub.with not same length")



# Split text into paragraphs and into sentences and only take the first few words of each sentence
p <- unlist(strsplit(text, "\n"))
l <- list()
for(i in seq_along(p)) {
	x <- unlist(strsplit(p[[i]], "\\. "))
	
	# rejoin at digits
	if(length(x)>0){
	for(k in length(x):1) if(substr(x[k],nchar(x[k]), nchar(x[k])) %in% 0L:9L ) {
		x[k] <- paste(x[k] , x[k+1], sep = ". ")
		x <- x[-(k+1)]
		}}

	x <- strsplit(x, " ")
	p2 <- character()
	for(j in seq_along(x)) p2 <- c(p2, paste(x[[j]][1:ifelse(length(x[[j]])<words, length(x[[j]]), words)], collapse = " "))
	if(length(p2)!=0) l[[i]] <- paste(paste0("*",p2,"...*"), collapse = "\n")
}


for(i in length(l):1) if(is.null(l[[i]])) l[[i]] <- NULL
r <- paste(unlist(l), collapse = "\n\n")

# return(r)
if(!is.null(write.file)) write.table(r, file = write.file, quote = F, row.names = F, col.names = F)  else return(cat(r))

}
