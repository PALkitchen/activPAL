build.anonymise.mapping <-
  function(folder){
    file_names <- list.files(folder, pattern="*.csv",recursive = FALSE)
    file_names <- file_names[grep("EventsEx",file_names)]
    file_names <- as.character(sapply(file_names, parse.file.name))
    mapping <- as.character(generate.anonymous.id(file_names))
    conversion_table <- data.frame(file_names,mapping,row.names = 1:length(file_names))
    conversion_table$file_names <- as.character(conversion_table$file_names)
    conversion_table$mapping <- as.character(conversion_table$mapping)
    return(conversion_table)
  }

generate.anonymous.id <-
  function(id_list){
    replace_list <- id_list
    randomized_prefix <- paste(rep(LETTERS, each = 26),rep(LETTERS,26),sep="")
    prefix <- substr(unique(id_list),1,regexpr("-",unique(id_list)))
    randomized_prefix <- randomized_prefix[1:length(prefix)] 
    randomized_prefix <- paste(randomized_prefix,"-",sep="")
    for(i in (1:length(randomized_prefix))){
      replace_list[grep(prefix[i],replace_list)] <- gsub(prefix[i],randomized_prefix[i],replace_list[grep(prefix[i],replace_list)])
    }
    return (replace_list)
  }

anonymise.id <- 
  function(id_list, mapping){
    for (i in (1:nrow(mapping))){
      id_list[which(id_list == mapping[i,]$file_names)] <- mapping[i,]$mapping
    }
    return (id_list)
  }