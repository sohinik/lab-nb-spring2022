#installation_needed <- TRUE
package_list <- c('data.table', 'here','OptimalDesign')
#if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
invisible(lapply(package_list, require, character.only = TRUE))

# x1: wage, x2: commute time x3: applications
# wage = {10,15,20,25}, commute time = {10,20,30,40,50,60}, application types ={1,2,3,4,5,6}


############

# od_KL

############

# Computes an optimal or near-optimal exact design of experiments under the standard (size) constraint on the size of the experiment.

# specify regression model

form.q <- ~ x1 + x2 + x3 

# specify the level of attributes 
Fx <- Fx_cube(form.q, lower = c(10,10,1), upper = c(25,60,6), n.levels = c(4,6,6))

# size = 5
w <- od_KL(Fx, 5, t.max = 8, crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design

# size = 10
w <- od_KL(Fx, 10, t.max = 8, crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design

# size = 15
w <- od_KL(Fx, 15, t.max = 8, crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design

# for continuous variables
# fine-tune the positions of the design points
# using general-purpose continuous optimization method
F <- Fx[rep(1:nrow(Fx), w), ]
f <- function(x) {c(1, x)}
# 
obj <- function(x, M.red) {-log(det(M.red + f(x) %*% t(f(x))))}
for (j in 1:size) {
        F[j, ] <- f(optim(F[j, 2:4], obj, M.red = t(F[-j, ]) %*% F[-j, ],
                 method = "L-BFGS-B", lower = c(10,10,1), upper = c(25,60,6))$par)
   }
tune <- od_pool(round(F, 4), rep(1, size))
Fx.tune <- tune$X.unique
w.tune <- tune$val.pooled
od_print(Fx.tune, w.tune, Fx.tune[,2:4])$design



###########

# od_RC 

###########

# Computes an efficient exact design under multiple linear resource constraints using the RC heuristic.
# A D-efficient exact design constrained by the total time and the total cost of the experiment.
# The linear constraint is given by Aw <= b
# The cost of a single trial in (x1, x2, x3) is  x1 + x2 + x3
# The limit on the total cost is 250,500,1000
# (we do not know the number of trials in advance)

# specify regression model
form.quad <-  ~x1 + x2 + x3
# specify attribute levels
Fx <- Fx_cube(form.quad, lower = c(10, 10,1), upper = c(25, 60,6), n.levels = c(4,6,6))
n <- nrow(Fx)
A <- matrix(0, nrow = 1, ncol = n)
for(i in 1:n) {A[1, i] <-  Fx[i, 2] + Fx[i, 3] + Fx[i,4]}

# b = 250
w <- od_RC(Fx, 250, A, bin = TRUE, t.max = 8,crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design

# b = 500
w <- od_RC(Fx, 500, A, bin = TRUE, t.max = 8,crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design

# b = 1000
w <- od_RC(Fx, 1000, A, bin = TRUE, t.max = 8,crit = "D")$w.best
od_print(Fx, w, Fx[,2:4])$design








