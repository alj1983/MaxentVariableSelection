source("Subsetselection.R")
source("Correlations.R")
source("Maxentrun.R")
source("ExtractVariables.r")

maxent="../../maxent.jar"
outdir="../../20150402Test"
gridfolder="/media/alj/DropboxWork/ScientificJournal/2011ScientificJournal/SDM/GridsForSDM/PresentDay"
occurrencesites="../../20150401_Fd_WithOslo_e_r.csv"
backgroundsites="../../20150401_BackgroundPoints_e_r.csv"
additionalargs="-b 0.5 nolinear noquadratic noproduct nothreshold -a 'raw' -z autorun"

coefficientthreshold <- 0.9
lowerexclusionthreshold <- 5


VariableSelection <- function(maxent,outdir,gridfolder,occurrencesites,backgroundsites,additionalargs,lowerexclusionthreshold,coefficientthreshold){
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
    while(length(already.tested.variables)<length(variablenames)){
                                        # As long as there are still
                                        # important variables for
                                        # which correlation was not
                                        # yet tested, continue with the variable reduction steps
        
      Maxentrun(maxent = maxent,
                  outdir = outdir,
                  gridfolder = gridfolder,
                  occurrencesites = occurrencesites,
                  backgroundsites = backgroundsites,
                  additionalargs = additionalargs)
                  
        
                                        # Getting the contributions of environmental variables
        variablecontributions <- Subsetselection(outdir)
        
        beginning.variable.set <- names(variablecontributions) # The set of
                                        # variables before
                                        # any is removed in this round
        
                                        # Extracting the variable contributions for the set of all environmental variables
        matching.variables <- numeric(length(beginningvariableset))
        matching.variables[matching.variables==0] <- NA
        matching.variables[match(names(variablecontributions),beginningvariableset)] <- variablecontributions
        matching.variables <- unlist(matching.variables)
        
        cat(c("Contributions",matching.variables,"\n"), file = paste(outdir,"/VariableSelectionProcess.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
        
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
        
        
        cat(c("CorrelationCoefficients",matching.correlations,"\n"), file = paste(outdir,"/VariableSelectionProcess.txt",sep=""), sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
                                        # The names of uncorrelated
                                        # variables. This is the set
                                        # of remaining variables
        uncorrelated.variables <- correlations[[2]]
        
                                        # Creating new csv files that contain only the uncorrelated variables
        ExtractVariables(uncorrelated.variables,occurrencesites,backgroundsites)

  }

    # Transform the table that lists Variable selection processes
    SelectionProcessTable <- read.table(paste(outdir,"/VariableSelectionProcess.txt",sep=""),header=TRUE)

    SelectionProcessTable.transposed <- t(SelectionProcessTable)
    
     write.table(SelectionProcessTable.transposed, file = paste(outdir,"/VariableSelectionProcess.txt",sep="") , append = FALSE, quote = FALSE, sep = "\t",
                 eol = "\n", na = "NA", dec = ".", row.names = TRUE,
                 col.names = FALSE)
}




VariableSelection(maxent,outdir,gridfolder,occurrencesites,backgroundsites,additionalargs,lowerexclusionthreshold,coefficientthreshold)
