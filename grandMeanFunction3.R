### Title:    Grand Mean Function & Friends
### Author:   Pavel Panko
### Created:  2018-MAR-01
### Modified: 2018-MAR-01

grandMean <- function(stackedData,
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
        dataNames     = names(stackedData),
        providedNames = providedNamesVec
    )
    ##
    ## III.  
    ##
    preData <- processData(
        data      = stackedData,
        dropNames = dropNamesVec,
        contNames = contNames
    )
    ##
    ## IV. Aggregate grand mean
    ##
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
        "keepNames"
    )
    names(mainArgNames) <- mainArgNames
    ##
    designList <- list()
    ##
    mainArgCheckLogic <- map(
        .x = mainArgNames,
        .f = ~ .x %in% names(functionCall)
    )
    mainArgCheckLogic %$%
        if(!idName) {
            stop("Please provide an id name.")
        } else if(!contNames & !discNames) {
            stop("Neither contNames nor discNames provided, please supply one of the two.")
        }
    mainArgCheckLogic %$%
        if (contNames) {
            contExist <<- TRUE
            designList$varNames$contNames <<- get("contNames", parent.frame(10))
            designList$funList$doMean <<- get("doMean", parent.frame(10))
        } else if (!contNames) {
            contExist <<- FALSE
            warning("contNames not provided, will only aggregate across discNames.")
        }
    ##
    mainArgCheckLogic %$%
        if (keepNames) {
            keepExist <<- TRUE
            designList$varNames$keepNames <<- get("keepNames", parent.frame(10))
            designList$funList$doKeep <<- get("doKeep", parent.frame(10))
        } else if (!keepNames) {
            keepExist <<- FALSE
        }
    ##
    mainArgCheckLogic %$%
        if (discNames) {
            discExist <<- TRUE
            designList$varNames$discNames <<- get("discNames", parent.frame(10))
            designList$funList$doMode <<- get("doMode", parent.frame(10))
        } else if (!discNames) {
            discExist <<- FALSE
            warning("discNames not provided, will only aggregate across contNames.")
        }  
    ##    
    mainArgCheckLogic %$%
        if (dropNames) {
            dropExist <<- TRUE
            assign("dropNamesVec", paste0(dropNames, collapse = "|"), envir = parent.frame(10))
        } else if (!dropNames) {
            dropExist <<- FALSE
            assign("dropNamesVec", TRUE, envir = parent.frame(10))
        } 
    ##
    mainArgCheckClass <- map(
        .x = functionCall,
        .f = class
    )
    mainArgCheckClass %$%
        if(idName != 'character'|length(id) > 1) {
            stop('Invalid "ID" argument - needs to be a character string of length 1.')
            ##   } else if(contExist & contNames != 'character') {
            ##       stop('Invalid "contNames" argument - needs to be a character string.')
            ## } else if(discExist & discNames != 'character') {
            ##     stop('Invalid "discNames" argument - needs to be a character string.')
            ## } else if(dropExist & dropNames != 'character') {
            ##     stop('Invalid "dropNames" argument - needs to be a character string.') 
            ## } else if(keepNames & keepNames != 'character') {
            ##     stop('Invalid "keepNames" argument - needs to be a character string.') 
        }
    ##
    assign("designList", designList, envir = parent.frame())
    providedNamesVec <- c(functionCall$idName, unlist(designList$varNames))
    assign("providedNamesVec", providedNamesVec, envir = parent.frame())
}
    
processNames <- function(dataNames, providedNames) {
    ##
    if(length(dataNames) > length(providedNames)) {
        stop("Not all the names in the data have been provided")
    } else if (length(dataNames) < length(providedNames)) {
        stop("Too many names have been provided")
    } else if (any(duplicated(dataNames))) {
        stop("There are duplicate names in the data")
    } else if (any(duplicated(providedNames))) {
        stop("There are duplicates in the provided names")
    } else if (length(dataNames) == length(providedNames)) {
        nameMatch <- any(is.na(match(dataNames, providedNames)))
        if(nameMatch) {
            stop("The names in the data and names provided do not line up")
        }
    }
    assign("dataNamesVec", dataNames, envir = parent.frame())
}

processData <- function(data, dropNames, contNames) {
    ##
    if(!all(dropNames)) {
        data %<>%
            select(-matches(dropNames))
        ##
        assign("dataNamesVec", names(data), envir = parent.frame())
    }
    ##
    data %<>%
        mutate_if(is.factor, as.character)
    ##
    checkCont <- all(map_lgl(data[,contNames], is.numeric))
    if(!checkCont) {
        warning("Some variables supplied in contNames are non-numeric - setting as numeric")
        data[,contNames] %<>%
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
    ## tmpMode <- as.numeric(mode)
    ## if(!is.na(tmpMode)) mode <- tmpMode
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
