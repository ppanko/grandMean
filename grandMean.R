### Title:    Grand Mean Function & Friends
### Author:   Pavel Panko
### Created:  2015-MAY-31
### Modified: 2016-JUN-07

grandMean <- function(data, contNames = NULL, discNames = NULL,
                      idName, fileName, fileType = ".csv", superMatrix = FALSE,
                      na = "NA", rowNames = TRUE, colNames = TRUE) {

    if(class(data) == "list") data <- do.call(rbind.data.frame, data)
    else if(class(data) != "data.frame") stop("Please provide data in list or data frame format")

    ## Create objects for name variables
    nameList <- list()
    oNames <- colnames(data)

    ## Set ID variable aside
    idVar <- data[,idName]
    dat <- data[, -which(idName %in% oNames)]

    ## Check if continuous & discrete names exist
    if(length(contNames) < 1|length(discNames) < 1) {

        ## If they do not, set discrete variables and continuous variables as factor/character and numeric/integer variables, respectively
        nameList$cont <-
            unique(c(names(Filter(is.factor, dat)), names(Filter(is.character, dat))))
        nameList$disc <-
            unique(c(names(Filter(is.numeric, dat)), names(Filter(is.integer, dat))))

    } else {

        ## If they do exist, insert them into the name list
        nameList$cont <- contNames
        nameList$disc <- discNames

    }

    ## Select functions to aggregate with
    funList <- list(Mode2, mean)

    ## Aggregate by variable type with the appropriate function
    gMeanTry <-
        mapply(aggData, data = list(dat), substs = nameList, func = funList, id = list(idVar), SIMPLIFY = FALSE)
    names(gMeanTry) <- NULL #Remove discrete/continuous classification (makes consistent naming much easier, lol)

    ## Transform aggergated list to data frame
    gMeanPrep <- do.call(cbind.data.frame, gMeanTry)

    ## Set the first "grouping variable" as the original id variable
    colnames(gMeanPrep)[1] <- paste0(idName)

    ## Remove other grouping variables
    gMeanPrep <- gMeanPrep[,-grep("Group.1", colnames(gMeanPrep))]

    ## Organize the names such that they are the same as in the original data
    cNames <- oNames[oNames %in% colnames(gMeanPrep)]
    gMeanData <- gMeanPrep[cNames]

    ## Set grand mean data to be saved
    saveDat <- gMeanData

    if(superMatrix == TRUE) {

        ## Calculate the covariances between the variables
        covMat <- cov(gMeanData, use = "complete.obs")

        ## Add mean and standard deviation rows to the top of the covariance matrix to create a SUPER MATRIX
        supMat <- rbind(colMeans(gMeanData, na.rm = TRUE), colSd(gMeanData), covMat) # P3 has doubts about this step

        ## Set super matrix data to be saved
        saveDat <- supMat

    }

    ## Save the file (depends on "fileType" argument)
    if(fileType == ".rds") {

        saveRDS(saveDat,
                file = paste0(fileName,
                              fileType)
                )

    } else if (fileType == ".csv") {

        write.csv(saveDat,
                  file = paste0(fileName,
                                fileType),
                  na = missing,
                  row.names = rowNames,
                  col.names = colNames
                  )

    } else if (fileType == ".dat") {

        write.table(saveDat,
                    file = paste0(fileName,
                                  fileType),
                    na = missing,
                    row.names = rowNames,
                    col.names = colNames
                    )

}

## Function for the mode
Mode2 <- function(x) {
    tab <- table(x, exclude = NULL)
    mode <- names(tab)[tab == max(tab)]

    if(length(mode) > 1) {
        mode <- mode[sample(c(1 : length(mode)), 1)]
    }

    return(mode)

}

## Function for aggregating data
aggData <- function(data, substs, func, id) {
    aggDat <- aggregate(data[,substs], by = list(id), FUN = func)
    return(aggDat)
}

## Function for calculating column standard deviations
colSd <- function(x) apply(X = x, MARGIN = 2, FUN = sd, na.rm = TRUE)

