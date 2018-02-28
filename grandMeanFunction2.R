### Title:    Grand Mean Function & Friends
### Author:   Pavel Panko
### Created:  2015-MAY-31
### Modified: 2018-FEB-28

grandMean <-
    function(data,
             idName, 
             contNames   = NULL,
             discNames   = NULL,
             dropNames   = NULL,
             save        = TRUE,
             fileName,
             separator   = ",",
             superMatrix = FALSE,
             missing     = "NA",
             rowNames    = FALSE,
             colNames    = TRUE)
{
    ##
    ## I. Set up workspace
    ##
    ## Check for dplyr in packages
    pkg <- c("rlang", "dplyr")
    lapply(
        pkg, function(p) {
            pkgCheck <- p %in% installed.packages()[, "Package"]
            ## If dplyr is missing, install
            if(!pkgCheck)
                install.packages(p)
        }
    )
    ## Load dplyr
    lapply(pkg, library, character.only = TRUE)  
    ## Check for dataframeness 
    if(class(data) != "data.frame")
        stop("Please provide data in 'data.frame' format")
    ##
    ## II. Set up "names" objects & check them
    ##
    ## Create objects for name variables   
    allNames <- colnames(data)
    assign("nameList", list(contNames, discNames))
    ## Check specified column names against all column names
    specifiedNames <- c(idName, dropNames, unlist(nameList))
    colNameCheck <- length(specifiedNames) == length(allNames)
    ## If lengths differ, produce error
    if(!colNameCheck)
        stop("Please make sure you have labeled all columns")
    ##
    ## III. Aggregate grand mean
    ##    
    ## Perform operation by variable type using the appropriate function
    gMeanPrep <-
        do.call(
            cbind,
            Map(
                f        = aggData,
                data     = list(data),
                subset   = nameList,
                func     = funList,
                id       = idName
            )
        )
    ##
    ## IV. Re-align data
    ##
    ## Bind data back together 
    gMeanData <- cbind(
        data[1:nrow(gMeanPrep),c(idName, dropNames)],
        gMeanPrep
    ) 
    ## Organize the names in the original order
    cNames <- allNames[allNames %in% colnames(gMeanData)]
    saveData <- gMeanData <- gMeanData[cNames]
    ##
    ## V. Wrap up
    ##
    ## Do supermatrix, if necessary 
    if(superMatrix) {
        ## Use superMatrix function on current grand-mean data
        saveData <- superMatrix(data = gMeanData)
    }
    ## Save, if necessary 
    if(save) {
        ## Use writeOut function
        writeOut(
            data = saveData,
            file = fileName,
            rows = rowNames,
            cols = colNames,
            na   = missing, 
            sep  = separator
        )
        ## If "save" is not specified...
    } else if (!save) {
        ## Return the final data object to environment
        return(saveData)
    }
}

## Function for the mode
Mode2 <- function(x) {
    ##
    tab <- table(x, exclude = NULL)
    mode <- names(tab)[tab == max(tab, na.rm = TRUE)]
    ## KML 2016-DEC-03: Convert to numeric, if possible
    tmpMode <- as.numeric(mode)
    if(!is.na(tmpMode)) mode <- tmpMode
    ##
    if(length(mode) > 1) {
        mode <- mode[sample(c(1 : length(mode)), 1)]
    }

    return(mode)

}

## Select functions to aggregate with
funList <- list(mean, Mode2)

## Function for aggregating data
aggData <- function(data, subset, func, id) {
    ##
    idVar <- data[,id]
    ##
    if(class(idVar) == "factor"| class(idVar) == "character")
        data[,id] <- as.numeric(as.character(idVar))
    else if (class(data[,id]) != "numeric")
        stop("Non-numeric id column")
    ##
    aggData <-
        suppressWarnings(
            data %>%
            filter(matches(paste0(id, subset, collapse = "|"))) %>%
            group_by_(id) %>%
            summarise_all(funs(func))## %>%
            ##select_(paste0("-", id))
        )
    ##
    return(aggData)
}

## Function for calculating column standard deviations
colSd <- function(x)
    apply(X = x, MARGIN = 2, FUN = sd, na.rm = TRUE)

superMatrix <- function(gMeanData) {
    ## Calculate the covariances between the variables
    covMat <- cov(
        data,
        use = "complete.obs"
    )
    ## Add mean and standard deviation rows to the top of the covariance matrix to create a SUPER MATRIX
    superMat <- rbind(
        colMeans(data, na.rm = TRUE),
        colSd(data),
        covMat
    ) # P3 has doubts about this step
    ## Set super matrix data to be saved
    return(superMat)
}

writeOut <- function(data, file, rows, cols, na, sep) {
    write.table(
        x         = data,
        file      = file,
        na        = na,
        row.names = rows,
        col.names = cols,
        sep       = sep
    )
}
