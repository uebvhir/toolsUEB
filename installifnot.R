
######################This script checks if the necessary packages to run the app are installed. 
######################If not, it proceeds to installation

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")


if (!require(DT)){
  install.packages(DT)
}
    
if (!require(shinycssloaders)){
  install.packages(shinycssloaders) 
}    

if (!require(RColorBrewer)){
  install.packages(RColorBrewer)  
}    

if (!require(colourpicker)){
  install.packages(colourpicker) 
} 
    
if (!require(gplots)){
  install.packages(gplots)
} 

if (!require(ggplot2)){
  install.packages(ggplot2)
}

if (!require(calibrate)){
  install.packages(calibrate)
} 

if (!require(xtable)){
  install.packages(xtable) 
} 

if (!require(digest)){
  install.packages(digest) 
} 

if (!require(limma)){
  BiocManager::install("limma")
} 

    
if (!require(GOfuncR)){
  BiocManager::install("GOfuncR")
} 

if (!require(TxDb.Hsapiens.UCSC.hg19.knownGene)){
  BiocManager::install("TxDb.Hsapiens.UCSC.hg19.knownGene")
} 


if (!require(Homo.sapiens)){
  BiocManager::install("Homo.sapiens")
} 


if (!require(stringi)){
  BiocManager::install("stringi")
} 
  
if (!require(ggrepel)){
  BiocManager::install("ggrepel")
}
  
  


