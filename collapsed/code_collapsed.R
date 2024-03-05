# NOTE: models include variables of project orders and issue matching

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
library(statnet)



#======SECTION 1. INPUT AND FILTER DATA
review_net_all <- readRDS("/share/crsp/lab/ulibarri/jiew35/collapsed/review_net_all_collapsed.rds")
dim(review_net_all)   # 62626     2


#======SECTION 2. INPUT ATTRIBUTES
Attributes_all <- readRDS("/share/crsp/lab/ulibarri/jiew35/collapsed/Attributes_all_collapsed.rds")
dim(Attributes_all)   #5368   13



#======SECTION 3. CREATE A NETWORK AND ASSIGN ATTRIBUTES

    # 1) set up a network
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
    }
}



#======SECTION 4. TWO-MODE ERGM -----------------------------   
 
#Table A4. Analysis of the CEQA Review Network with Regional Agencies Collapsed into a Single Large Agency




#models with dyads
tableA4 <- ergm(teamnet_all ~      
           
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
    write.csv(as.data.frame(coefficients(summary(tableA4))), file="/share/crsp/lab/ulibarri/jiew35/collapsed/tableA4.csv") 

summary(tableA4)


saveRDS(tableA4, "/share/crsp/lab/ulibarri/jiew35/collapsed/tableA4.rds")


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
   
       
           "b2factor.year.2011" = "Year: 2011",
           "b2factor.year.2012" = "Year: 2012",
           "b2factor.year.2013" = "Year: 2013",
           "b2factor.year.2014" = "Year: 2014",    
           "b2factor.year.2015" = "Year: 2015",    
           "b2factor.year.2016" = "Year: 2016",    
           "b2factor.year.2017" = "Year: 2017",    
           "b2factor.year.2018" = "Year: 2018",
           "b2factor.year.2019" = "Year: 2019",    
           "b2factor.year.2020" = "Year: 2020",


           "gwb2deg.fixed.0.6" = "GW (project) degree",

           "edges"   = "Edges"    
)    

tr2 <- extract(tableA4, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE)    
      

    htmlreg(tr2,
          custom.coef.map = map, 
          file = "/share/crsp/lab/ulibarri/jiew35/collapsed/tableA4.doc",
          single.row = TRUE,
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          caption = "Table A4. Analysis of the CEQA Review Network with Regional Agencies Collapsed into a Single Large Agency",
        include.nobs = TRUE,
          caption.above = TRUE)




#GOF
      
    pdf("/share/crsp/lab/ulibarri/jiew35/collapsed/cmc_tableA4.pdf")
    mcmc.diagnostics(net)
    dev.off()  
   

gof_all_model <- gof(tableA4, GOF = ~model )
gof_all_model
png(filename= "/share/crsp/lab/ulibarri/jiew35/collapsed/gof_TableA4.png", width=1500,height=500)
plot(gof_all_model, cex.axis=1.5, cex.lab= 2, las = 3)
dev.off() 


      
