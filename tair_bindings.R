TAIR.bindings<-function(aliases, bindings, save = FALSE){
    alias.frame<-read_delim(aliases, delim="\t")
    bind.frame<-read_delim(bindings, delim = "\t")
    
    bind.frame[,1]<-apply(bind.frame, 1, function(x){
        to.sub<-substr(x[1], 1, nchar(x[1])-2)
        y<-sub("AT.G\\d+\\.\\d", to.sub , x[1])
        return(y)
    })
    
    ###
    full.frame<-unique(
        merge(x = alias.frame, y = bind.frame, by = "name", all=TRUE)
        )
    # this merges two files from TAIR into one which has a lot of useful info
    ###
    
    ###
    full.frame<- full.frame %>%
        group_by(name) %>%
        summarise_all(funs(toString(unique(na.omit(.)))))
    # this is borrowed from a forum. It uses dplyr package to combine rows by the AT name and fills gaps form multiple sources
    ###
    
    if(save == TRUE){
        write.table(full.frame, file = "tair_bindings.txt", sep = "\t", row.names = FALSE)
        print("file saved to current directory as tair_bindings.txt")
    }
    

    return(full.frame)
}