### Title:    Grand Mean Function & Friends
### Author:   Pavel Panko
### Created:  2018-MAR-01
### Modified: 2018-MAR-01

grandMean <- function(data,
                      idName, 
                      contNames   = NULL,
                      discNames   = NULL,
                      dropNames   = NULL,
                      keepNames   = NULL)
{
    ##
    if(class(data) != "data.frame")
        stop("Please provide data in 'data.frame' format")
    ##
    functionCall <- as.list(sys.call())
    ##
    ## I. 
    ##
    ## Check for dplyr in packages
    processArgs(functionCall)
    ##
    ## II. 
    ##
    processNames(
        dataNames     = names(data),
        providedNames = unlist(nameList)
    )
    ##
    ## III.  
    ##
    preData <- processData(
        data      = data,
        dropNames = dropNamesVec
    )
    ##
    ## IV. Aggregate grand mean
    ##
    designList$funsList = list(
        doMean = doMean,
        doMode = doMode,
        doKeep = doKeep
    )
    ## Perform operation by variable type using the appropriate function
    postData <- aggregateData(
        data      = preData,
        id        = idName,
        design    = designList,
        dataNames = dataNamesVec
    )
    ##
    return(postData)
}

##
processArgs <- function(functionCall) {
    ##
    mainArgNames <- c(
        "idName",    
        "contNames",  
        "discNames",  
        "dropNames",  
        "save",       
        "fileName"
    )
    names(mainArgNames) <- mainArgNames
    ##
    designList <- list()
    ##
    mainArgCheckLogic <- map_lgl(
        .x = mainArgNames,
        .f = ~ .x %in% names(functionCall)
    ) %$%
        if(!idName) {
            stop("Please provide an id name.")
        } else if(!contNames & !discNames) {
            stop("Neither contNames nor discNames provided, please supply one of the two.")
        } else if (!contNames) {
            warning("contNames not provided, will only aggregate across discNames.")
            cont <- FALSE
        } else if (contNames) {
            cont <- TRUE
            designList$varNames$contNames <- contNames
        } else if (!discNames) {
            warning("discNames not provided, will only aggregate across contNames.")
            disc <- FALSE
        } else if (discNames) {
            disc <- TRUE
            designList$varNames$discNames <- discNames
        } else if (keepNames) {
            designList$varNames$keepNames <- keepNames
        } else if (!dropNames) {
            assign("dropNamesVec", FALSE, envir = parent.frame())
        } else if (dropNames) {
            assign("dropNamesVec", paste0(dropNames, collapse = "|"), envir = parent.frame())
        } 
    ##
    mainArgCheckClass <- map(
        .x = functionCall,
        .f = class
    ) %$%
        if(idName != 'character'|length(id) > 1) {
            stop('Invalid "ID" argument - needs to be a character string of length 1.')
        } else if(contNames != 'character') {
            stop('Invalid "contNames" argument - needs to be a character string.')
        } else if(discNames != 'character') {
            stop('Invalid "discNames" argument - needs to be a character string.')
        } else if(dropNames != 'character') {
            stop('Invalid "dropNames" argument - needs to be a character string.') 
        } else if(save != 'logical'|length(save) > 1) {
            stop('Invalid "save" argument - needs to be a logical vector of length 1.')
        }
    ##
    assign("designList", designList, envir = parent.frame())
}

processNames <- function(dataNames, providedNames) {
    ##
    if(length(dataNames) > length(providedNames)) {
        stop("Not all the names in the data have been provided")
    } else if (length(dataNames) < length(providedNames)) {
        stop("Too many names have been provided")
    } else if (any(duplicates(dataNames))) {
        stop("There are duplicate names in the data")
    } else if (any(duplicates(providedNames))) {
        stop("There are duplicates in the provided names")
    } else if (length(dataNames) == length(providedNames)) {
        nameMatch <- any(is.na(match(dataNames, providedNames)))
        if(!nameMatch) {
            stop("The names in the data and names provided do not line up")
        }
    }
    assign("dataNamesVec", dataNames, envir = parent.frame())
}

processData <- function(data, dropNames) {
    ##
    if(!all(dropNames)) {
        data %<>%
            select(-matches(dropNames))
    }
    ##
    data %<>%
        mutate_if(is.factor, as.character)
    ##
    checkCont <- all(map_lgl(data[contNames], is.numeric))
    if(!checkCont) {
        warning("Some variables supplied in contNames are non-numeric - setting as numeric")
        data[contNames] %<>%
            mutate_if(is.character, as.numeric)
    }
    ##
    return(data)
}


## Function for aggregating data
aggregateData <- function(data, id, design, dataNames) {
    ##
    grandMeanData <- design %>%
        pmap(
            ~ data %>%
                group_by_(id) %>%
                summarise_at(.x, .y)
        ) %>%
        reduce(inner_join, by = id) %>%
        select(order(dataNames))
    ## 
    return(grandMeanData)
}

##
doMean <- function(x) {
    ##
    mean <- mean(x, na.rm = TRUE)
    return(mean)
}

## Function for the mode
doMode <- function(x) {
    ##
    tab <- table(x, exclude = NULL)
    mode <- names(tab)[tab == max(tab, na.rm = TRUE)]
    ##
    if(!is.na(tmpMode)) mode <- tmpMode
    ##
    if(length(mode) > 1) {
        mode <- mode[sample(1:length(mode), 1)]
    }
    ##
    return(mode)
}

doKeep <- function(x) {
    ##
    keep <- head(x, 1)
    return(keep)
}

##
checkPackages <- function() {
    pkg <- c("rlang", "dplyr", "magrittr")
    lapply(
        pkg, function(p) {
            pkgCheck <- p %in% installed.packages()[, "Package"]
            ## If dplyr is missing, install
            if(!pkgCheck)
                install.packages(p)
        }
    )
    ## Load pkgs
    lapply(pkg, library, character.only = TRUE)
}


checkPackages()
