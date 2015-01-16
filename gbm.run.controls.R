library(knitr)
library(gbm)
library(dplyr)

#load the dataset
data <- 
load(data)


#read in variable selection
inputs <- 

#create formulas
input.var <- lapply(inputs,function(x) inputs[x == 1,'var'])[c(10)]
response.var <- 'outcome'
form <- as.matrix(sapply(input.var, function(x) paste(response.var,' ~ offset(log(time1 - time0)) +',
                                    paste(x,c(rep('+',length(x)),''),collapse = ' '),collapse = '')))

#train the model
for (rows in 1:nrow(form)) {
  for (num.trees in 2 * 1000) {
    for (depth in 3) {
      for (shrink in .05) {
        
        #fit models
        start.time <- Sys.time()
        set.seed(14159265)
        fit.gbm <- gbm(as.formula(form[rows]),data = theDta,
                         distribution = 'poisson',cv.folds = 5,shrinkage = shrink,
                         n.trees = num.trees,interaction.depth = depth)
        
        #create output directory
        directory <- paste('directory',
                           row.names(form)[rows],'.d',depth,'.t',num.trees,'.s',shrink,'.',Sys.Date(),sep = '')
        dir.create(directory)
        
        #save model      
        save(fit.gbm,file = paste(directory,'/',row.names(form)[rows],'.',Sys.Date(),'.RData',sep = ''))
        
        #save data dictionary
        #dict <- attr(theDta,'data.dictionary')
        #save(dict,file = paste(directory,'/','data.dictionary','.RData',sep = ''))
        
        #save report
        opts_knit$set(base.dir = directory)
        knit2html(input = 'directory',
                            output = paste(directory,'/',row.names(form)[rows],'.',Sys.Date(),'.html',sep = ''))
        
        paste0('The model runtime was ',format(as.numeric(Sys.time() - start.time)/60,digits = 2),' minutes.')
      }
    }
  }
}
