library(tidyverse)
try(source(fs::path(DirRepo.eval,"./functions/calc.lins.ccc.R")), silent=T)
#source(fs::path(DirRepo.eval,"./functions/calc.lins.ccc.R"))

linear_terms <- function( x_col, y_col , df, site, var1, var2){
  
  subset_df = df %>% select( y_col, x_col) %>% drop_na(c(y_col, x_col))
  
  if( subset_df[,1] %>% length > 2) {
    linear.summary <- lm( data = df, df[,y_col] %>% as.numeric ~ df[,x_col]%>% as.numeric ) %>% summary
    
    linear.term.df <- data.frame(Site = site,
                                 x= x_col,
                                 y=y_col,
                                 intercept = linear.summary$coefficients[1] ,# Intercept
                                 slope = linear.summary$coefficients[2], # slope
                                 R2=linear.summary$r.squared ,
                                 CCC = calc.lins.ccc(subset_df[[x_col]], subset_df[[y_col]])$rho.c$est,
                                 var1= var1,
                                 var2=var2)# rsquared 
    
  }
  
  if( subset_df[,1] %>% length <= 2) {
    linear.term.df <- data.frame(Site = site,
                                 x= x_col,
                                 y=y_col,
                                 intercept = NA,# Intercept
                                 slope = NA, # slope
                                 R2=NA ,
                                 CCC = NA,
                                 var1= var1,
                                 var2= var2)# rsquared 
  }
  
  return(linear.term.df )
}

linear_terms2 <- function( x_col, y_col , df, site){
  
  subset_df = df %>% select( y_col, x_col) %>% drop_na(c(y_col, x_col)) %>% as.data.frame
  subset_df$WP
  

  if( subset_df[,1] %>% length > 2) {
    
    linear.summary <- lm( data =  subset_df,  subset_df[,1] ~  subset_df[,2]%>% as.numeric ) %>% summary
    
    linear.term.df <- data.frame(Site = site,
                                 x= x_col,
                                 y=y_col,
                                 intercept = linear.summary$coefficients[1] ,# Intercept
                                 slope = linear.summary$coefficients[2], # slope
                                 R2=linear.summary$r.squared ,
                                 CCC = calc.lins.ccc(subset_df[,2], subset_df[, 1])$rho.c$est)# rsquared 
    
  }
  
  if( subset_df[,1] %>% length <= 2) {
    linear.term.df <- data.frame(Site = site,
                                 x= x_col,
                                 y=y_col,
                                 intercept = NA,# Intercept
                                 slope = NA, # slope
                                 R2=NA ,
                                 CCC = NA)# rsquared 
  }
  
  return(linear.term.df )
}
