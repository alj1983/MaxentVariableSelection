\name{VariableSelection}
\alias{VariableSelection}
\title{Selecting the best set of relevant environmental variables for MAXENT Niche Modeling}
\description{
Complex niche models show low performance in identifying the most
important range-limiting environmental variables and in transferring
habitat suitability to novel environmental conditions. This package
helps to identify the most important set of uncorrelated variables and
to fine-tune Maxent's regularization multiplier. In combination, this
allows to constrain complexity and increase perforance of Maxent niche
models (assessed by the area under the receiver operating characteristic
(AUC) and information criteria as AICc).
%% ~~ A concise (1-5 lines) description of what the function does. ~~ }
\usage{

VariableSelection(maxent, outdir, gridfolder, occurrencesites,
backgroundsites, additionalargs, lowerexclusionthreshold,
coefficientthreshold, betamultiplier)

}
\arguments{


  \item{maxent}{String specifying the filepath to the maxent.jar file (download from here: https://www.cs.princeton.edu/~schapire/maxent/). The package was tested with maxent.jar version 3.3.3k.
  }
    \item{outdir}{String specifying the path to the output directory to which all the result files will be written.
    }
      \item{gridfolder}{String specifying the path to the directory that holds all the ASCII grids (in ESRI’s
  .asc format) of environmental variables. All variables must have the same extent and resolution.
      }
        \item{occurrencesites}{String specifying the filepath to the csv
  file with occurrence records. Please find the exact specifications of
  the SWD file format in the details section below.
	}
	  \item{backgroundsites}{String specifying the filepath to the
  csv file with background/pseudoabsence data. Please find the exact
  specifications of the SWD file format in the details section below.
	  }
	    \item{additionalargs}{String specifying additional maxent
	      arguments. Please see in the details section below. 
	      
	    }
	      \item{betamultiplier}{Vector of beta (regularization
  multipliers) (positive numerical values). The smaller this value, the
  more closely will the projected distribution fit to the training data
  set. Overfitted models are poorly transferable to novel environments
  and, thus, not appropriate to project distribution changes under
  environmental change. The model performance will be compared between
  models created with the beta values given in this \code{betamultiplier}
  vector. Thus, providing a range of beta values from 1 (the default in
  Maxent) to 15 or so, will help you to spot the optimal beta multiplier
  for your specific model.
	      }
	        \item{coefficientthreshold}{Numerical value (between 0
  and 1) that sets the threshold of Pearson's correlation coefficient above
  which environmental variables are regarded to be correlated (based on
	      values at all background locations). Of the
	      correlated variables, only the variable with the highest
	      contribution score will be kept, all other correlated
	      variables will be excluded from the Maxent model.
	      Correlated variables should be removed because they may reflect the
   same environmental conditions, and can lead to overly complex or
   overpredicted models. Also, models comiled with correlated variables might give wrong predictions in scenarios where the
   correlations between the variables differ.
 }
		  \item{lowerexclusionthreshold}{Numerical value
  (between 0 and 100) that sets the threshold of model contributions
  below which environmental variables are excluded from the Maxent
	      model. Model contributions reflect the importance of
	      environmental variables in limiting the distribution of
	      the target species.
}
}



\details{
  \bold{SWD file format of occurrence and background sites}
  
The files with occurrence sites and with background sites must be saved
in SWD format. This format contains not only longitude and latitude
values but also values of all environmental variables for each location.
The file should be saved in csv format, which means that values are
separated by commas. Here are some example lines from a file with
background sites, with the first line as header line:

 
\code{species,longitude,latitude,calcite,salinity,maxtemperature\cr
bg,39.79,65.29,0.008,34.577,10.28\cr
bg,21.12,79.54,0.014,22.214,1.28\cr
bg,-4.95,55.29,0.001,29.077,15.64\cr
bg,-135.37,59.29,0.006,31.59,12.05\cr
bg,18.04,42.62,0.001,33.575,25.93\cr
bg,-4.45,48.70,0.003,31.614,15.62\cr}

Have also a look at the datasets \code{Backgrounddata} and \code{Occurrencedata}
that are included in this package. If you have longitude and latitude
data of your occurrence or background sites, you can extract values of
environmental variables from ASCII rasters with the extract function of
the R package 'raster' to create files of the SWD format. 

\bold{Additional MAXENT arguments}

MAXENT arguments can be specified with the \code{additionalargs} string.
You find an overview of possible arguments in the end of the help file
that opens when you click the 'Help' button in the graphical user
interface of MAXENT (which can be opened with the command 'java -jar
maxent.jar' or by clicking on the maxent.jar file).

The following settings are preset in this package to calculate
information criteria and AUC values. Don't change these values in the
\code{additionalargs} string:

\code{autorun=true}: starts running with the startup of the program.\cr
\code{plots=false}: doesn't write any plots.\cr
\code{writeplotdata=false}: don't write output data that are required to
plot response curves.\cr
\code{visible=false}: don't show the Maxent user interface but run the
programm barely from the command line.\cr
\code{randomseed=true}: the background data will be split into different
test/train partitions with each new run.\cr
  \code{writebackgroundpredictions=false}: don't write .csv files with
  predictions at background points.\cr
  \code{replicates=10}: number of replicate runs; this is only set for the calculation of AUC values.\cr
  \code{replicatetype=subsample}: use random test/training data for
  replicated sample sets; this is only set for the calculation of AUC values.\cr
  \code{randomtestpoints=50}: 50 percent of presence points are randomly
  set aside as test data; this is only set for the calculation of AUC values.\cr
  \code{redoifexists}: redo the analysis if output files exist already.\cr
  \code{writemess=false}: doesn't write multidimensional similarity
  surfaces (MESS), which inform on novel climate conditions in the
  projection layers.\cr
  \code{writeclampgrid=false}: don't write a grid on the spatial
  distribution of clamping.\cr
  \code{askoverwrite=false}: don't ask if the data shall be
  overwritten if they exist already.\cr
  \code{pictures=false}: don't create .png images of the output grids\cr
  \code{outputgrids=false}: don't write output grids for all replicate
  runs but only the summary grids; this is only set for the calculation of AUC values.\cr
  \code{outputformat=raw}: write output grids with raw probabilities of
  presence; this is only set for calculation of information criteria.\cr
  
  What you might want to choose, for example, are the features to be
  used, by selecting true or false for the arguments
    \code{linear}, \code{quadratic}, \code{product}, \code{threshold},
  and \code{hinge}.

\bold{Model selection procedure}

First, an initial MAXENT model is compiled with all provided
environmental variables. Then, all variables are excluded which have a
relative contribution score below the value set with
\code{lowerexclusionthreshold}. Then, those variables are removed that
corrleate with the variable of highest contribution (at a correlation
coefficient > \code{coefficientthreshold} or <
-\code{coefficientthreshold}).  The remaining set of variables is then
used to compile a new MAXENT model. Again, variables with low
contribution scores are removed and remaining variables that are
correlated to the variable of second-highest contribution are
discarded. This process is repeated until left with a set of
uncorrelated variables that all had a model contribution above the value
set with \code{lowerexclusionthreshold}.  These steps are perfored for
the range of beta-multipliers specified with \code{betamultiplier}.


The performance of each MAXENT model is assessed with the sample-size-
adjusted Akaike information criterion (AICc) (Akaike, 1974), the area
under the receiver operating characteristic (AUC) estimated from test
data (Fielding and Bell, 1997), and the difference between AUC values
from test and training data (AUC.Diff), which estimates model
overfitting. AICc values are estimated from single models that
include all occurrence sites, based on code in ENMTools (Warren et al.,
2010). AUC values, instead, are averaged over ten replicate runs which
differ in the set of 50\% test data that are randomly sub-sampled from
the occurrence sites and withheld from model construction. Maximization
of test AUC values generally favors models that can well discriminate
between presence and absence sites (Fielding and Bell, 1997; Warren and
Seifert, 2011). This, however, bears the risk to overpredict the
realized niche of a species (Jiménez-Valverde, 2012).

Instead, minimization of AICc values generally favors models that
recognize the fundamental niche of a species and that are better
transferable to future climate scenarios (Warren and Seifert, 2011).
The optimal set of variables along with the best beta multiplier is then
identified as the model of highest AUC or lowest AICc values (the output
gives you both options)


%% ~~ If necessary, more details than the description above ~~ }
\value{

The following result files are saved in the directory specified with the \code{outdir}
argument. 

\item{ModelPerformance.txt}{A table listing the performance indicators
  of all created Maxent models
  \describe{
    \item{\code{Model}}{Unique model number}
    \item{\code{betamultiplier}}{Maxent's regularization multiplier}
    \item{\code{variables}}{Number of environmental variables}
    \item{\code{samples}}{Number of occurrence sites}
    \item{\code{parameters}}{Number of parameters estimated from the model}
    \item{\code{loglikelihood}}{log likelihood value}
    \item{\code{AIC}}{Akaike Information Criterion}
    \item{\code{AICc}}{sample size corrected AIC}
    \item{\code{BIC}}{Bayesian information criterion}
    \item{\code{AUC.Test}}{Area under the receiver operating
      characteristic fro test data}
    \item{\code{AUC.Train}}{Area under the receiver operating
      characteristic fro training data}
    \item{\code{AUC.Diff}}{Difference between AUC.Test and AUC.Train}
  }
  
  The information criteria (AIC, AICc, and BIC) are set to 'x' if the
  number of parameters is lower than the number of variables in the model.
  
}


\item{ModelSelectionAICc_MarkedMaxAUCTest.png}{
  A figure showing the AICc
  values of all models, which are ordered along the x-axis based on the applied
  beta-multiplier. The number of environmental variables included in
  each model is coded by dot color and size. The model with highest
  AUC.Test value is marked in red.
}

\item{ModelSelectionAICc_MarkedMinAICc.png}{ 
  A figure showing the AICc
  values of all models, which are ordered along the x-axis based on the applied
  beta-multiplier. The number of environmental variables included in
  each model is coded by dot color and size. The model with highest
  minimum AICc value is marked in red. 
}

\item{ModelSelectionAUCTest_MarkedMaxAUCTest.png}{
  A figure showing the AUC.Test
  values of all models, which are ordered along the x-axis based on the applied
  beta-multiplier. The number of environmental variables included in
  each model is coded by dot color and size. The model with highest
  AUC.Test value is marked in red.
}

\item{ModelSelectionAUCTest_MarkedMinAICc.png}{
  A figure showing the AUC.Test
  values of all models, which are ordered along the x-axis based on the applied
  beta-multiplier. The number of environmental variables included in
  each model is coded by dot color and size. The model with highest
  minimum AICc value is marked in red.
}

\item{ModelWithMaxAUCTest.txt}{
  Subset of the table \code{ModelPerformance.txt}, which shows only the
  model with the highest AUC.Test value.
}

\item{ModelWithMinAICc.txt}{
  Subset of the table
  \code{ModelPerformance.txt}, which shows only the model with the
  lowest AICc value.
}


\item{X_VariableSubset.csv}{
  \code{occurrencesites} and \code{backgroundsites} input files reduced to
  the final set of selected environmental variables. 'X' stands for the
  input file names of the csv files with occurrence and background data.
  \describe{
    \item{\code{species}}{species name}
    \item{\code{longitude}}{longitude coordinate}
    \item{\code{latitude}}{latitude coordinate}
    \item{\code{X}}{values for each of the environmental variables that
      were selected as optimal set. Here, 'X' stands for the name of one
      or more input variables.}
  }
}

\item{VariableSelectionProcess.txt}{Table listing model contributions for
  and correlations between each of the environmental variables for all
  created Maxent models. The numbers of the models refer to the unique
  model numbers in the table \code{ModelPerformance.txt} (see above).
  The following entries describe the content row-wise (not column-wise):
  \describe{
    \item{\code{Test}}{Either 'Contributions' or 'Correlation. Informs if the numbers for each of the
      environmental variables refers to model contribution coefficients
      or to correlation coefficients.}
    \item{\code{Model}}{The unique model number (the same unique model
      number in \code{ModelPerformance.txt.}}
    \item{\code{betamultiplier}}{The (regularization
      multipliers) used to compile the respective model.}
    \item{\code{X}}{'X' stands here for the name of an environmental
      variable. The \code{Test} row above informs whether the values in this row refer to the model
      contribution of this environmental variable or to its coefficient
      of correlation with another environmental variable. The variable
      to which it is compared is recognizable by a correlation
      coefficient of 1. If this environmental variable was excluded from
      the model, the value in this row is 'NA', which stands for 'Not Available'.}
  }
}

\item{VariableSelectionMaxAUCTest.txt}{Subset of
  \code{VariableSelectionProcess.txt} that shows only those models which
  lead directly to the model with the  highest AUC.Test value.
  \describe{
  }
}

\item{VariableSelectionMinAICc.txt}{Subset of
  \code{VariableSelectionProcess.txt} that shows only those models which
  lead directly to the model with the lowest AICc value.
  \describe{
  }
}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...


\references{

Akaike H (1974)\cr
A new look at the statistical model identification\cr
\emph{IEEE Transactions on Automatic Control} \bold{19}:6 716--723.

Fielding AH \& Bell JF (1997)\cr
A review of methods for the assessment of prediction\cr
errors in conservation presence/absence models\cr
\emph{Environmental Conservation} \bold{24}:1 38--49.

Jim\'{e}nez-Valverde A (2012)\cr
Insights into the area under the receiver operating characteristic curve\cr
(AUC) as a discrimination measure in species distribution modelling\cr
\emph{Global Ecology and Biogeography} \bold{21}:4 498--507.

Warren DL, Glor RE, \& Turelli M (2010)\cr
ENMTools: a toolbox for comparative studies of environmental niche
models\cr
\emph{Ecography} \bold{33}:3 607--611.

Warren DL \& Seifert SN (2011)\cr
Ecological niche modeling in Maxent: the importance of model\cr
complexity and the performance of model selection criteria\cr
\emph{Ecological Applications} \bold{21}:2 335--342.

}



\author{Alexander Jueterbock, \email{Alexander-Jueterbock@web.de}\cr
%%  ~~who you are~~
}

\section{Warning}{Depending on the number of environmental variables and
  the range of different betamultipliers you want to test, variable
  selection can take several hours so that you might want to run the
  analysis over night.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
% change this to the location of the maxent.jar file on your own computer
maxent <- system.file("extdata", "maxent.jar", 
package="RpackageVariableSelection")
% XX See in which  directory I should normally put the maxent.jar file

outdir="Test"

gridfolder <- system.file("extdata/grids/",
package="RpackageVariableSelection")

occurrencesites <- system.file("extdata", "Occurrencedata.csv", 
package="RpackageVariableSelection")

backgroundsites <- system.file("extdata", "Backgrounddata.csv", 
package="RpackageVariableSelection")

% unselect all but hinge features with the following additional
% arguments
additionalargs="nolinear noquadratic noproduct nothreshold noautofeature"

% With the following selection, variable selection will run through the beta
% multipliers 4 and 4.5, 5, 5.5, and 6.
betamultiplier=seq(4,6.5,0.5)

coefficientthreshold <- 0.9

lowerexclusionthreshold <- 5

VariableSelection(maxent, outdir, gridfolder, occurrencesites,
backgroundsites, additionalargs, lowerexclusionthreshold,
coefficientthreshold, betamultiplier)


}
\keyword{nonparametric}

xx create also a vignette.
XX Rename lowerexclusionthreshold to contributionthreshold and
coefficientthreshold to correlationthreshold in all files.



XX I use system 'cp' in the VariableSelection function. This will not
work on windows.

Need to change in the Variable selection function which tables to
keep. Change Occurrencesites.csv to
paste(occurrencesitefilename,"_VariableSubset.csv",sep="")
and keep also the Backgroundsites_VariableSubset.csv file.