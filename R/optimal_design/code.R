#####################################
# Date: April 2022

# This R file generate optimal designs for conjoint choice experiments for specified prior distributions

# variables of interest
# wage = {12,15,20,25}, commute time = {15,30,45,60}, 
# application types = {"no information", "be one of the first to apply", "0-4 applications",
# "5-20 applications", "21-50 applications", "51-200 applications", "200+ applications"}

# Algorithm: Coordinate Exchange Algorithm (CEA) from "idefix" package
# CEA conducts changes on an attribute-by-attribute basis and is efficient for large choice sets

# Reference: Traets, F., Sanchez, D. G., & Vandebroek, M. (2020). Generating optimal designs for discrete choice experiments in R: the Idefix package. Journal Of Statistical Software, 96, 1-41.

####################################


installation_needed <- TRUE
package_list <- c('data.table', 'here', "idefix")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
invisible(lapply(package_list, require, character.only = TRUE))



# number of choice sets per prior
k <- 18 

# generate set of priors for 5 application dummies; each can take three values {-0.3,0,0.3}
l <- rep(list(c(-0.3,0,0.3)),5)
applications <- as.matrix(expand.grid(l))





# ------ function to generate optimal design -----------

optimal_design <- function(app, k = 18){
  
  at.lvls <- c(4, 4, 6)       # number of levels for each attribute
  c.type <- c("C", "C", "D")           # whether an attribute is continuous or discrete
  con.lvls <- list(c(12, 15, 20 ,25), c(15, 30, 45, 60))         # levels for continuous variables
  mu_other_var <- c(0, 0, 0.066, -0.17)         # prior mean: alternative specific constants (asc), wage and commuting time
  mu <- c(mu_other_var, app)         # prior for five app dummies can take 3^5 = 243 sets of values 
  se <- c(0.2, 0.2, 0.02, 0.005, 0.35, 0.35, 0.35, 0.35, 0.35)     
  sigma <- diag(se^2) 
  set.seed(123)
  M <- MASS::mvrnorm(n = 500, mu = mu, Sigma = sigma)       # draw 500 random samples from prior distributions
  m <- list(M[,1:2], M[,3:9])        # to avoid confusions, split asc and other variables into a list of two vectors
  
  # generate optimal design
  D <- CEA(lvls = at.lvls, coding = c.type, c.lvls = con.lvls,  alt.cte = c(0,1,1), n.sets = k, n.alts = 3,  par.draws = m) 
  
  # decode to make attribute levels reader friendly
  
  lvls <- list(c("$12", "$15", "$20", "$25"), c("15 min","30 min","45 min","60 min"),
               c("no information","be one of the first to apply", "0-4 applications", "5-20 applications", "51-200 applications", "200+ applications"))
 -
  DD <- Decode(des = D$design, lvl.names = lvls, coding = c.type, c.lvls = con.lvls, alt.cte = c(0,1,1), n.alts = 3)
 ss
  # convert optimal design to a data frame
  data <- as.data.frame(DD$design)
  return(data)
}





# ----- apply optimal design function to sets of priors -----

start_time <- Sys.time()

for (i in 1:nrow(applications)){
  print(i)
  # each row of application matrix represents a set of priors for each applications dummy, e.g. {-0.3,0,0,0,0}
  if (i == 1){
    mydata <- optimal_design(app = applications[i,], k = 18)
    } 
  else{
    mydata <- rbind(mydata, optimal_design(app = applications[i,], k = 18))
    }
}

end_time <- Sys.time()

end_time - start_time





# ----- format final data set ---------

# number of priors 
A <- nrow(applications) 

# add prior columns to the data set
mydata <- cbind(mydata,applications %x% rep(1,3*k))

# columns {wages, commute_time, applications} represent optimal design of choice sets, and applications categories are values of priors
colnames(mydata) <- c("wage","commute_time","applications",
                      "be one of the first to apply", "0-4 applications", "5-20 applications", "51-200 applications", "200+ applications")

# id for each prior; total prior_id 3^5 = 243 
mydata$prior_id <- seq(1:A) %x% rep(1,3*k)

# individual_id for choice sets; total 18 choice sets per prior
mydata$individual_id <- rep(seq(1:k) %x% rep(1,3),A)

# id for job alternatives; total 3 options per choice set
mydata$jobs <- rep(c("jobA","jobB","jobC"),k*A)

# reorder id variables to the first columns; drop idefix helper variable V1
optimal_design <- mydata[,c(10:12,2:9)] 





# ------- save data -----------
write.csv(optimal_design, here("optimal_design.csv"), row.names = FALSE)

optimal_design <- fread(here("optimal_design.csv"))
