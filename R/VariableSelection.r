VariableSelection <- function(maxent,outdir,gridfolder,occurrencesites,backgroundsites,additionalargs,lowerexclusionthreshold,coefficientthreshold){

    library("raster")
# the first three columns in the input tables must have the header 'species','longitude', and 'latitude'
    
                                        # Extracting the set of variables of the original csv table
    beginningvariableset <- colnames(read.csv(occurrencesites,header=TRUE))
                                        # Excluding the first three columns which contain no variables, but
                                        # the species name, longitudes and latitudes of occurrence sites
    beginningvariableset <- beginningvariableset[4:length(beginningvariableset)]
    beginning.variable.set <- beginningvariableset # the very first set of variables

    uncorrelated.variables <- character() # No variable testing was
                                          # yet done

    
                                        # Start a file that shows the single steps in the variable selection process
    cat(c("Variable",beginningvariableset,"\n"), file = paste(outdir,"/VariableSelectionProcess.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = FALSE)

    cat(c("Model","samples","parameters","loglikelihood","AIC","AICc","BIC","AUC.Test","AUC.Train","AUC.Diff","\n"), file = paste(outdir,"/ModelPerformance.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = FALSE)

                                        # Extract only the name and strip off the filepath from the occurrence and background site files
    occurrencesitefilename <- gsub(".*/","",occurrencesites)
    backgroundsitefilename <- gsub(".*/","",backgroundsites)

                                        #  Make a copy of the occurrence and backgroundsites in order not to overwrite the original files
    system(paste("cp", occurrencesites, paste(outdir,"/",occurrencesitefilename,"_VariableSubset.csv",sep=""),sep=" "))
    system(paste("cp", backgroundsites, paste(outdir,"/",backgroundsitefilename,"_VariableSubset.csv",sep=""),sep=" "))

    occurrencesites <- paste(outdir,"/",occurrencesitefilename,"_VariableSubset.csv",sep="")
    backgroundsites <- paste(outdir,"/",backgroundsitefilename,"_VariableSubset.csv",sep="")


    already.tested.variables <- character() # Creating an empty
                                            # character vector that
                                            # will be filled with the
                                            # names of variables for
                                            # which correlation to
                                            # other variables has
                                            # already been tested.

#    while (length(beginning.variable.set)>length(uncorrelated.variables)){
    
                                        # As long as this is true, variables had been removed in the
                                        # last step and thus, more variables might be redundant.
    variablenames <- beginningvariableset

    # Number the models
    modelnumber <- 1
    
    while(length(already.tested.variables)<length(variablenames)){
                                        # As long as there are still
                                        # important variables for
                                        # which correlation was not
                                        # yet tested, continue with the variable reduction steps

                                        # Maxentrun for AUC test value
                                        # calculation. The preset
                                        # options options keep the
                                        # number of created files to a
                                        # minimum (only the
                                        # maxentResult.csv file is
                                        # needed) and to create with
                                        # every run a new set of
                                        # random test points.
        Maxentrun(maxent = maxent,
                  outdir = outdir,
                  gridfolder = gridfolder,
                  occurrencesites = occurrencesites,
                  backgroundsites = backgroundsites,
                  additionalargs = paste("plots=false writeplotdata=false randomseed=true autorun=true writebackgroundpredictions=false replicates=10 replicatetype=subsample randomtestpoints=50 redoifexists writemess=false writeclampgrid=false askoverwrite=false pictures=false outputgrids=false ",
                      additionalargs,sep="")
                )
         AUCs <- MaxentAUC(outdir)
        
                                        #Maxentrun for information
                                        #criteria calculation. The
                                        #preset options reduce the
                                        #number of unnecessary output
                                        #files. Information criteria
                                        #calculation requires only raw
                                        #output grids, a lambdas file
                                        #and the latitude and
                                        #longitude data of occurrence
                                        #sites.

        Maxentrun(maxent = maxent,
                  outdir = outdir,
                  gridfolder = gridfolder,
                  occurrencesites = occurrencesites,
                  backgroundsites = backgroundsites,
                  additionalargs = paste("plots=false writeplotdata=false randomseed=true autorun=true writebackgroundpredictions=false redoifexists writemess=false writeclampgrid=false askoverwrite=false pictures=false outputformat=raw ",
                      additionalargs,sep="")
                  )

        occurrencesites.data <- read.csv(occurrencesites,header=TRUE)
        species <- as.character(occurrencesites.data$species[1])
        lambdas <- paste(outdir,"/",species,".lambdas",sep="")
        gridfolder.without.path <- gsub(".*/","",gridfolder)
        pred.raw <- paste(outdir,"/",species,"_",gridfolder.without.path,".asc",sep="")
        ICs <- MaxentIC( occurrencesites, pred.raw, lambdas)

        cat(c(modelnumber,as.vector(ICs),AUCs,abs(diff(AUCs)),"\n"), file = paste(outdir,"/ModelPerformance.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
        
        variablecontributions <- Subsetselection(outdir)
        
        beginning.variable.set <- names(variablecontributions) # The set of
                                        # variables before
                                        # any is removed in this round
        
                                        # Extracting the variable contributions for the set of all environmental variables
        matching.variables <- numeric(length(beginningvariableset))
        matching.variables[matching.variables==0] <- NA
        matching.variables[match(names(variablecontributions),beginningvariableset)] <- variablecontributions
        matching.variables <- unlist(matching.variables)
        
        cat(c(paste("Contributions_Model",modelnumber,sep=""),matching.variables,"\n"), file = paste(outdir,"/VariableSelectionProcess.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
        
                                        # selecting the set of variables that exceed a user-defined contribution threshold
        selected.variables <- variablecontributions[,variablecontributions>lowerexclusionthreshold]
        
                                        # the names of selected variables
        variablenames <- colnames(selected.variables)
                                        # The first variable name belongs to the most important variable
                                        # because they were sorted by variable contribution
        
                                        # Exclude the variables for which correlation was already tested
        remaining.variables <- variablenames[variablenames%in%already.tested.variables==FALSE]
        
        important.notyettested.variable <- remaining.variables[1]
                                        # As the variablenames were sorted by importance, the first
                                        # remaining one has highest contribution to the model
        
                                        # add this variable to the set
                                        # of variables for which
                                        # correlation to other
                                        # variables was already thested
        already.tested.variables <- c(already.tested.variables,important.notyettested.variable)
                                        # Getting a list with  correlation coefficients of all selected variables and the names of uncorrelated variables
        correlations <- Correlations(important.notyettested.variable,variablenames,backgroundsites,coefficientthreshold)
        
                                        # Extracting the correlation coefficients for the set of all environmental variables
        matching.correlations <- numeric(length(beginningvariableset))
        matching.correlations[matching.correlations==0] <- NA
        matching.correlations[match(names(correlations[[1]]),beginningvariableset)] <- correlations[[1]]
        matching.correlations <- unlist(matching.correlations)
        
        
        cat(c(paste("CorrelationCoefficients_Model",modelnumber,sep=""),matching.correlations,"\n"), file = paste(outdir,"/VariableSelectionProcess.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
                                        # The names of uncorrelated
                                        # variables. This is the set
                                        # of remaining variables
        uncorrelated.variables <- correlations[[2]]
        
                                        # Creating new csv files that contain only the uncorrelated variables
        ExtractVariables(uncorrelated.variables,occurrencesites,backgroundsites)

        # Increase the model number by 1
        modelnumber <- modelnumber+1

  }

    # Transform the table that lists Variable selection processes
    SelectionProcessTable <- read.table(paste(outdir,"/VariableSelectionProcess.txt",sep=""),header=TRUE)

    SelectionProcessTable.transposed <- t(SelectionProcessTable)
    
     write.table(SelectionProcessTable.transposed, file = paste(outdir,"/VariableSelectionProcess.txt",sep="") , append = FALSE, quote = FALSE, sep = "\t",
                 eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                 col.names = FALSE)
}

