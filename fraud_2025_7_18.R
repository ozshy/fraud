# Code for fraud_v?.tex titled: "Whistleblowers and Financial Fraud"
# model simulations

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
#library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displayes multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
#library("xtable") #exporting to LaTeX
#library(dplyr)# for sample_n
#library(gtools)# for stars.pval function

setwd("~/Papers/fraud/fraud_R_Derive")
dir()

#Start simulation of Figure 5 (probabilities as function of C)####

# Model parameters 
# fraud parameters
(L = 120) #fraud level
(rho = 0.8) #fraction of L recovered if convicted
(phi = 40) #penalty if convicted
#(phi = 80) #penalty if convicted
#
# Utility parameters
(delta = 2) #WB discomfort 
(gamma = 1) #utility concavity parameter

# compensation level
(C.vec = 0:10)# will change after figuring the exact range to have both probabilities in [0,1]

# verify assumption that ensures an interval of C with prob in [0,1]
(1-rho)*L
phi > (1-rho)*L

# Finding the range of compensation C with prob in [0,1]
# pw < 1 if C <
(2*delta*(phi +rho*L)^2)/L^2
# pf < 1 if C > 
(2*delta*L)/(phi+rho*L)
#therefore set C in the range
(C.vec = seq(3,6,0.1))# allowing > 1 on the edges

# Derivations
# eq (9)
(pw.vec = ((C.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(C.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*C.vec^2*(phi+rho*L)^2))^(1/3))

# drawing the 3 prob (Figure 5 in paper)
(prob.df = data.frame(C.vec, pw.vec, pf.vec, lambda_bar.vec))
#
ggplot(prob.df, aes(x=C.vec)) + geom_line(aes(y=pw.vec)) +geom_line(aes(y=pf.vec, color="red"))+geom_line(aes(y=lambda_bar.vec, color="blue"))+scale_y_continuous(breaks = seq(0,1.1,0.1))

#end simulation of Figure 5 (probabilities as function of C)####

#Start 1st simulation of expected loss as function of C####
# Using the same parameters as for Figure 5 except:
(C.vec = seq(3.6,5.1,0.1))# ensuring both probabilities < 1
#
# Derivations
# eq (9)
(pw.vec = ((C.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(C.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*C.vec^2*(phi+rho*L)^2))^(1/3))

# eq (11): expected fraud loss 
(eloss.vec = pf.vec*(L -pw.vec *lambda_bar.vec *(rho*L -C.vec +phi)))
is.unsorted(eloss.vec)# True if non-monotonic
is.unsorted(-eloss.vec)# True if non-monotonic
which.min(eloss.vec)
which.max(eloss.vec)
# => Expected loss is minimized at C_max where pw=1

#End 1st simulation of expected loss as function of C####

#Start 2nd Simulation of expected loss as function of C####
# Model parameters 
# fraud parameters
(L = 120) #fraud level
(rho = 0.7) #fraction of L recovered if convicted
(phi = 40) #penalty if convicted
#
# Utility parameters
(delta = 2) #WB discomfort 
(gamma = 1) #utility concavity parameter

# compensation level
(C.vec = 0:10)# will change after figuring the exact range to have both probabilities in [0,1]

# verify assumption that ensures an interval of C with prob in [0,1]
(1-rho)*L
phi > (1-rho)*L

# Finding the range of compensation C with prob in [0,1]
# pw < 1 if C <
(2*delta*(phi +rho*L)^2)/L^2
# pf < 1 if C > 
(2*delta*L)/(phi+rho*L)
#therefore set C in the range
(C.vec = seq(3.8,4.2,0.05))# this ensures all prob in [0,1]

# Derivations
# eq (9)
(pw.vec = ((C.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(C.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*C.vec^2*(phi+rho*L)^2))^(1/3))

# eq (11): expected fraud loss 
(eloss.vec = pf.vec*(L -pw.vec *lambda_bar.vec *(rho*L -C.vec +phi)))
is.unsorted(eloss.vec)# True if non-monotonic
is.unsorted(-eloss.vec)# True if non-monotonic
#
which.min(eloss.vec)
which.max(eloss.vec)
# => Expected loss is minimized at Cmin where pf = 1 and pw < 1.

#End 2nd Simulation of expected loss as function of C####
