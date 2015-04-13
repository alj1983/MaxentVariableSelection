Maxentrun <-
function(maxent,outdir,gridfolder,occurrencesites,backgroundsites,additionalargs){

     system(paste("nohup java -jar", maxent,
      "-o", outdir,
      "-j", gridfolder,
      "-s", occurrencesites,
      "-e", backgroundsites,
      additionalargs,"&",sep=" "),intern=TRUE)
      }
