MaxentAUC <-
function(outdir){
    Resulttable <- read.csv(paste(outdir,"/maxentResults.csv",sep=""),sep=",",header=TRUE)
    # Select those columns where the column name contains the word "Test AUC"
    TestAUCs <- Resulttable[,(colnames(Resulttable) %in% grep("Test.AUC",colnames(Resulttable),value=T))]
    # The last one of these values is the average value
    TestAUC.average <- TestAUCs[length(TestAUCs)]
    return(TestAUC.average)
    }
