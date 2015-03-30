Correlations <-
    function(important.variable,variablenames,backgroundsites,coefficientthreshold){
                                        # variables that are correlated with a correlation coefficient higher than the 'coefficientthreshold' will be  excluded
        
                                        # The first variablename is expected to be the variable with highest contribution to the maxent model
        
        
        backgroundvalues <- read.csv(backgroundsites,sep=",",header=TRUE)
                                        # select the specified variables from the backgroundvalues
        backgroundvalues <- backgroundvalues[,(colnames(backgroundvalues) %in% variablenames)]
        
                                        # calculate correlation coefficients
        correlation.coefficients <- cor(backgroundvalues)
        
                                        # Extract the correlation coefficients to the most important variable
        correlation.coefficients <- correlation.coefficients[,colnames(correlation.coefficients)==important.variable]
        
                                        # remove the values where the most important variable was correlated to itself
        correlation.coefficients <- correlation.coefficients[names(correlation.coefficients)!=important.variable]
        uncorrelatedvariables <-  names(correlations)[abs(correlation.coefficients)<coefficientthreshold]
        return(list(correlation.coefficients,uncorrelatedvariables))
    }
