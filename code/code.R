library(dplyr)
library(readr)
library(igraph)
library(stringr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(texreg)
library(rstatix)
library(ade4)
library(tnet)
library(ergm.tapered)
library(statnet)




#======SECTION 1. INPUT AND FILTER DATA
review_net_all <- readRDS("review_net_all.rds")
dim(review_net_all)


#======SECTION 2. INPUT ATTRIBUTES
Attributes_all <- readRDS("Attributes_all.rds")  
dim(Attributes_all)  # 5395   15


#======SECTION 3. CREATE A NETWORK AND ASSIGN ATTRIBUTES

    # 1) set up a network
# myfunction <- function(review_net, Attributes, save_name){
    data <-  review_net_all %>% select(`ra_recoded_abb`,`project_id`)# two-mode edgelist 
    g <-graph.data.frame(data,directed = FALSE) # test.net is changed to a graph here

    V(g)$type <- bipartite_mapping(g)$type 
    bipartite_matrix <- as_incidence_matrix(g)

    teamnet_all <- as.network(bipartite_matrix, directed=FALSE,  bipartite=TRUE)

#get degree
    # for table 1
    summary(degree_tm(bipartite_matrix))  #for agency
    summary(degree_tm(t(bipartite_matrix))) # for project

    ## 2) assign network node attirbutes

    teamnet_all%v%"ra_topic" <- Attributes_all$"ra_topic"  
    teamnet_all%v%"logemployee" <- log(Attributes_all$"employee")
    teamnet_all%v%"issues_combined" <- Attributes_all$"issues combined"                                   
    teamnet_all%v%"issues_n" <- Attributes_all$"N of issues"         


    teamnet_all%v%"la_level_two" <- as.numeric(factor(Attributes_all$"la_level_two", order = TRUE, levels =c('lower-state', 'higher-state')))  #1=lower; 2= state and higher


    table(teamnet_all%v%"la_level_two")


    teamnet_all%v%"document_type" <- Attributes_all$"Document Type"                        

    teamnet_all%v%"year" <- Attributes_all$"year"   


    teamnet_all%v% "logpop_den" <- log(Attributes_all$"Population Density (Per Sq. Mile)" ) 

   
    teamnet_all%v%"pbs"  <- Attributes_all$"Pollution Burden Score" 
    teamnet_all%v%"pcs" <- Attributes_all$"Pop. Char. Score"


    teamnet_all%v%"logpast_ten" <- log(Attributes_all$"past_ten"+1)


    # 3) assign network edge attirbutes


    # get ra topics
matrix_x <- Attributes_all %>% select(`ra_recoded_abb`,`ra_topic`)
matrix_x <- matrix_x[complete.cases(matrix_x), ]

    # get project issues
matrix_y <- Attributes_all %>% select(`project_id`,`issues combined`)
matrix_y <- matrix_y[complete.cases(matrix_y), ]


    # create a issue_matching matrix
edge_matrix <- bipartite_matrix

for (i in 1: nrow(bipartite_matrix)){
    for (j in 1:ncol(bipartite_matrix)){
        edge_matrix[i, j] <- grepl(matrix_x$ra_topic[i], matrix_y$"issues combined"[j], fixed = TRUE)
#         cat(' elemenet:', i, j)
    }
}


#======SECTION 4. TWO-MODE ERGM -----------------------------   
 
#models without dyads    (tableA3)

tableA3 <- ergm(teamnet_all ~      
           
           b1cov("logemployee") +
           b1cov("logpast_ten") +   
      
           
           b2cov("issues_n")  + 

           b2cov("pbs" )+  
           b2factor("document_type", levels=-3) +     

           b2factor("la_level_two",levels=-1) +   
                           
              
           b2cov("pcs") + 
           b2cov("logpop_den") +
           
           b2factor("year") +


           gwb2degree(0.6, fixed = TRUE) + 
           edges   

    , control = control.ergm(seed = 123, MCMLE.maxit = 200, MCMC.burnin=1000000000))

summary(tableA3)


saveRDS(tableA3, "tableA3.rds")



#models with dyads (Table 4)
table4 <- ergm(teamnet_all ~      
           
           b1cov("logemployee") +
           b1cov("logpast_ten") +   
      
           
           b2cov("issues_n")  + 

           b2cov("pbs" )+  
           b2factor("document_type", levels=-3) +     

           b2factor("la_level_two",levels=-1) +   
                           
              
           b2cov("pcs") + 
           b2cov("logpop_den") +
           
           b2factor("year") +

           edgecov (edge_matrix) +
           b1nodematch("ra_topic") +
           b2nodematch("issues_combined") + 

           gwb2degree(0.6, fixed = TRUE) + 
           edges   

    , control = control.ergm(seed = 123, MCMLE.maxit = 200, MCMC.burnin=1000000000))

summary(table4)


saveRDS(table4, "table4.rds")



#output
  
map <- list(
            "b1cov.logemployee" = "Agency Size (logged)",
            "b1cov.logpast_ten" = "Past Experience (logged)", 
           "b2cov.issues_n" = "Project Issues",
           "b2cov.pbs" =  "Pollution Burden Score",

           "b2factor.document_type.EIR" = "EIR",       
           "b2factor.document_type.MND" = "MND",
           "b2factor.document_type.NEG" = "NEG",

           "b2cov.pcs" = "Vulnerable Index",
           "b2cov.logpop_den" = "Population Density (logged)", 
   
           "b2factor.la_level_two.1" = "Lead Agency (<state)",
           "b2factor.la_level_two.2" = "Lead Agency (>=state)",  
      
           "edgecov.edge_matrix" = "Issue Matching",
           "b1nodematch.ra_topic" = "Knowledge Similarity",
           "b2nodematch.issues_combined" =  "Issue Similarity",
 
           "gwb2deg.fixed.0.6" = "GW (project) degree",

           "edges"   = "Edges"    
)    

tr1 <- extract(tableA3, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE)
tr2 <- extract(table4, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE)    
      

    htmlreg(tr1,
          custom.coef.map = map, 
          file = "tableA3.doc",
          single.row = TRUE,
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          caption = "Table A3. Analysis of the CEQA Review Network without Dyadic Covariates",
        include.nobs = TRUE,
          caption.above = TRUE)


    htmlreg(tr2,
          custom.coef.map = map, 
          file = "table4.doc",
          single.row = TRUE,
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          caption = "Table 4. Two-Mode ERGM Analysis of the CEQA Review Network",
        include.nobs = TRUE,
          caption.above = TRUE)




#GOF
      
    pdf("mcmc_table4.pdf")
    mcmc.diagnostics(table4)
    dev.off()  
   

# model is too large, so run gof seperately (Figure A1)

gof_all_model <- gof(table4, GOF = ~model )
gof_all_model
png(filename= "gof_model_table4.png", width=1500,height=500)
plot(gof_all_model, cex.axis=1.5, cex.lab= 2, las = 3)
dev.off() 

 

# #for degree2
mod_gof_degree2 <- gof(table4,GOF=~b2degree,control = list(nsim = 1e4))
saveRDS(mod_gof_degree2, "mod_gof_degree2.rds")


# mod_gof_degree2 <- readRDS("mod_gof_degree2.rds")  

# 0.025 and 0.975 quantiles of count at each degree value from 1k simulations
upper_lower_95 <- apply(mod_gof_degree2$sim.b2deg,2,quantile,c(0.025,0.975))
upper_lower_df <- data.frame(t(upper_lower_95))
names(upper_lower_df) <- c('0.025q','0.975q')
upper_lower_df$x <- (1:nrow(upper_lower_df))-1


png(filename= "gof_degree2.png")

ggplot() +
  geom_errorbar(data = upper_lower_df,
                aes(col = '95% of simulated values',ymin = `0.025q`,ymax = `0.975q`,x=x)) +
  geom_point(pch = 21,fill = 'white',aes(col = 'observed',x = as.numeric(str_extract(names(mod_gof_degree2$obs.b2deg),'[0-9]+$')),
  y = mod_gof_degree2$obs.b2deg)) +
  theme_bw() +
  scale_color_manual(values = c('black','black')) +
  scale_y_continuous(name = '# of project nodes with degree value X') +
  # scale_x_continuous(name = 'degree value (# agency-project ties)')+
  theme(legend.position = c(0.8,0.4), axis.title = element_text(size = 16)) +
  xlim(1, 27) + 

  # scale_x_continuous(name = 'degree value (# agency-project ties)')+
  labs(x = "degree value (# agency-project ties)") + 
guides(color = guide_legend(override.aes = list(size = 2,pch = c(NA,21)) ) )

dev.off() 

        


# calculate VIFs for collinearity (Table A2)

vif.ergm<-function(my.ergm){

  cor.mat<-stats::cov2cor(my.ergm$covar) #calculate correlation matrix
  corr5<-cor.mat[-c(1),-c(1)] ##omit edges term

  corr5<-corr5[!is.na(corr5[1:nrow(corr5)]),]
  corr5<-corr5[,which(!is.na(corr5[1,1:ncol(corr5)]))]

  VIFS<-matrix(0,nrow=1,ncol=ncol(corr5))

  for(i in 1:ncol(corr5)){

    gvec<-as.vector(corr5[-c(i),i]) ##create vector of correlations between covariate of interest and other covariates in the model
    tgvec<-t(gvec)
    xcor<-solve(corr5[-c(i),-c(i)]) ##create square matrix of correlations between covariates in the model other than the one of interest
    Rsq<-tgvec%*%xcor%*%gvec
    VIFS[1,i]<-1/(1-Rsq)
  }

    colnames(VIFS)<-names(my.ergm$coef[-c(23)])
 message("Higher values indicate greater correlation.\nVIF > 20 is concerning, VIF > 100 indicates severe collinearity.")
  VIFS
}



vif.ergm(table4)

vif <- t(vif.ergm(table4))
write.csv(as.data.frame(vif), file="vif_table4.csv") 

    
