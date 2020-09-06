capFirst <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}

# This function helps to source multiple files which are in the same directory. 
# Just provide it with a path and all .R files in the directory it is pointed
# to will be sourced. Can be done recursively or not.


sourceDiectory <- function(path , recursive = TRUE , local = TRUE){
        if(!dir.path(path)){
                warning(paste(path," is not a valid path!!"))
                return(NULL)
        }
        
        # Source it where function is called local
        if(is.logical(local) && local){
                env <- parent.frame()
        }# Source it where global environment
        else if(is.logical(local) && !local){
                env <- globalenv()
        }
        else if(is.environment(local)){
                env <- local()
        }
        else{
                stop("'local' must be TRUE , FALSE or an environment")
        }
        
        files <- list.files(path = path 
                            , full.names = TRUE 
                            , all.files = FALSE
                            , recursive = FALSE)
        
        for(aFile in files){
                tryCatch(
                        {
                                source(aFile , local = env)
                                cat(aFile," is sourced!")
                        },
                        error <- function(cond){
                                message("Failed loading the following file\" ", aFile ,"\".")
                                message(cond)
                        }
                )
        }
}