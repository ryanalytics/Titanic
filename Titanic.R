library('readxl')
library('openxlsx')
library('writexl')
library('psych')
library('ggplot2')
library('moments')
library('reshape2')
library('reticulate')
library('pivottabler')
library('tidyr')

#Use reticalute package to run python file that cleans data

#Sets location for python 
use_python(PyLoc, required=TRUE)
#Sets folder location for python file I am running 
setwd(PyFileLoc)
py_run_file('TitanicCleanUp.py')

