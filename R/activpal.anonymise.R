build.anonymous.mapping <-
  function(file_names){
    mapping <- as.character(generate.anonymous.id(file_names))
    conversion_table <- data.frame(file_names,mapping,row.names = 1:length(file_names))
    conversion_table$file_names <- as.character(conversion_table$file_names)
    conversion_table$mapping <- as.character(conversion_table$mapping)
    return(conversion_table)
  }

generate.anonymous.id <-
  function(id_list){
    replace_list <- data.frame(id_list)
    colnames(replace_list)[1] <- "prefix"
    randomized_prefix <- paste(rep(LETTERS, each = 26),rep(LETTERS,26),sep="")

    replace_list <- randomized_prefix[sample(length(id_list))]

    return (replace_list)
  }

anonymise.id <-
  function(id_list, mapping){
    for (i in (1:nrow(mapping))){
      id_list[which(id_list == mapping[i,]$file_names)] <- mapping[i,]$mapping
    }
    return (id_list)
  }
