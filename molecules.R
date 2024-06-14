

createSmiles <- function(mainMolecule, numberOfVariationSites, input) {
  
  library(gtools) #Import all libraries needed 
  library(roperators) 
  library(readxl) 
  library(writexl) 
  
  input <- read_excel(input)
  secondaryMolecules <- list()
  
  for (v in input) {
    
    for (s in v) {
      
      secondaryMolecules <- append(secondaryMolecules, s) #Get data from excel file and append it to secondaryMolecules
      
    }
    
  } 
  
  vec <- c(as.vector(unlist(secondaryMolecules))) #Turn list of secondaryMolecules into a Vector 
  
  
  res <- combinations(n= length(secondaryMolecules), r = numberOfVariationSites, v = vec, repeats.allowed=T) #Calculate combinations possible, repeats allowed, named "res" 
  
  combinations <- as.list(data.frame(t(res))) #Convert vectors of combinations, into list of combinations 
  
  
  smiles <- list() #List of SMILES 
  
  for (f in combinations) {
    
    vct <- as.list(unlist(strsplit(f, '[[:space:]]'))) #Seperate each of the SMILES 
    
    counter <- 1 
    
    smileMainMolecule <- mainMolecule #Create a new copy of the mainMolecule SMILE to work with and edit
    
    
    for (i in 1:numberOfVariationSites) {
      
      q <- paste("s", i, sep = "") 
      
      
      smileMainMolecule <- gsub(q, vct[counter], smileMainMolecule) #Find and substitute s1, s2, s3, ...
      counter <- counter + 1
      
      
    }
    
    smiles <- append(smiles, smileMainMolecule) #Append SMILES of molecule to list
    
    
  }
  
  
  data <- data.frame(Results = c(unlist(smiles))) #Covert list of SMILES into a dataframe 
  
  write_xlsx(data,"output.xlsx") #Output the SMILES to an XLSX file 
  
  
  
} 


