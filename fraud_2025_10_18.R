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
library("xtable") #exporting to LaTeX
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
(c.vec = 0:10)# will change after figuring the exact range to have both probabilities in [0,1]

# verify assumption that ensures an interval of C with prob in [0,1]
(1-rho)*L
phi > (1-rho)*L

# Finding the range of compensation C with prob in [0,1]
# pw < 1 if C <
(c_max = (2*delta*(phi +rho*L)^2)/L^2)
# pf < 1 if C > 
(c_min = (2*delta*L)/(phi+rho*L))
 #therefore set C in the range
(c.vec = seq(3,6,0.01))# allowing > 1 on the edges
(c.vec = seq(c_min,c_max,0.01))# not allowing

# Derivations
# eq (9)
(pw.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))

# drawing the 3 prob (Figure 5 in paper)
(prob.df = data.frame(c.vec, pw.vec, pf.vec, lambda_bar.vec))
#
ggplot(prob.df, aes(x=c.vec)) + geom_line(aes(y=pw.vec)) +geom_line(aes(y=pf.vec, color="red"))+geom_line(aes(y=lambda_bar.vec, color="blue"))+scale_y_continuous(breaks = seq(0,1.1,0.1))

# now, without lambda_bar
ggplot(prob.df, aes(x=c.vec)) + geom_line(aes(y=pw.vec)) +geom_line(aes(y=pf.vec, color="red"))+scale_y_continuous(breaks = seq(0,1.1,0.1))+ theme(legend.position="none")
# => seems like pf is convex, check 2nd derivative.

#end simulation of Figure 5 (probabilities as function of C)####

#Start 1st simulation of expected loss as function of C####
# Using the same parameters as for Figure 5 except:
(c.vec = seq(3,6,0.01))# allowing for probabilities > 1 on edges
(c.vec = seq(c_min,c_max,0.01))# not allowing
#
# Derivations
# eq (9)
(pw.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# also in eq (10) expected conviction prob Elambda
(lambda_bar.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))

# eq (11): expected fraud loss 
(eloss.vec = pf.vec*(L -pw.vec *lambda_bar.vec *(rho*L -c.vec +phi)))
is.unsorted(eloss.vec)# True if non-monotonic
is.unsorted(-eloss.vec)# True if non-monotonic
which.min(eloss.vec)
which.max(eloss.vec)
# => Expected loss is minimized at C_max where pw=1

# plot eloss.vec
eloss.df = data.frame(c.vec, eloss.vec)
ggplot(eloss.df, aes(x=c.vec, y=eloss.vec)) + geom_line()
# => convex => minimum loss is with at c_min or c_max

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
(c.vec = 0:10)# will change after figuring the exact range to have both probabilities in [0,1]

# verify assumption that ensures an interval of C with prob in [0,1]
(1-rho)*L
phi > (1-rho)*L

# Finding the range of compensation C with prob in [0,1]
# pw < 1 if C <
(c_max = (2*delta*(phi +rho*L)^2)/L^2)
# pf < 1 if C > 
(c_min = (2*delta*L)/(phi+rho*L))
#therefore set C in the range
(c.vec = seq(c_min, c_max, 0.005))# this ensures all prob in [0,1]

# Derivations
# eq (9)
(pw.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))

# eq (11): expected fraud loss 
(eloss.vec = pf.vec*(L -pw.vec *lambda_bar.vec *(rho*L -c.vec +phi)))
is.unsorted(eloss.vec)# True if non-monotonic
is.unsorted(-eloss.vec)# True if non-monotonic
#
which.min(eloss.vec)
which.max(eloss.vec)
# => Expected loss is minimized at Cmin where pf = 1 and pw < 1.

# plot eloss.vec
eloss.df = data.frame(c.vec, eloss.vec)
ggplot(eloss.df, aes(x=c.vec, y=eloss.vec)) + geom_line()
# => convex => minimum loss is with at c_min or c_max

#End 2nd Simulation of expected loss as function of C####

#Start 3rd Simulation of expected loss as function of C####
# trying to get INTERIOR which min eloss. trying changing delta
# Model parameters 
# fraud parameters
(L = 120) #fraud level
(rho = 0.8) #fraction of L recovered if convicted
(phi = 40) #penalty if convicted
#
# Utility parameters
(delta = 2) #WB discomfort 
(delta = 7) #WB discomfort 
(gamma = 1) #utility concavity parameter

# compensation level
(c.vec = 0:10)# will change after figuring the exact range to have both probabilities in [0,1]

# verify assumption that ensures an interval of C with prob in [0,1]
(1-rho)*L
phi > (1-rho)*L

# Finding the range of compensation C with prob in [0,1]
# pw < 1 if C <
(c_max = (2*delta*(phi +rho*L)^2)/L^2)
# pf < 1 if C > 
(c_min = (2*delta*L)/(phi+rho*L))
#therefore set C in the range
(c.vec = seq(c_min, c_max, 0.01))# this ensures all prob in [0,1]

# Derivations
# eq (9)
(pw.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# eq (10)
(pf.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# also in eq (10)
(lambda_bar.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))

# eq (11): expected fraud loss 
(eloss.vec = pf.vec*(L -pw.vec *lambda_bar.vec *(rho*L -c.vec +phi)))
is.unsorted(eloss.vec)# True if non-monotonic
is.unsorted(-eloss.vec)# True if non-monotonic
#
which.min(eloss.vec)
which.max(eloss.vec)
# => Expected loss is minimized at Cmin where pf = 1 and pw < 1.

# plot eloss.vec
eloss.df = data.frame(c.vec, eloss.vec)
ggplot(eloss.df, aes(x=c.vec, y=eloss.vec)) + geom_line()
# => convex => minimum loss is with at c_min or c_max

#End 3rd Simulation of expected loss as function of C####

#Start Table 3: Calibrations of gamma
(r.vec = seq(0.1, 0.8, 0.1))
length(r.vec)

(gammas.vec = 1/r.vec -1)
#
(gammam.vec = 1/r.vec -2)

# make it a table
(gamma.df = data.frame(r.vec, gammas.vec, gammam.vec))

print(xtable(gamma.df, digits = c(0,1,2,2)),  include.rownames=F)

# 
# # Start Figure 6 in the paper: Combing 2 new simulations (cancelled!) ####
# (c.vec = seq(3.5,5.2,0.01))# => wider than needed, but c_min and c_max will be plotted in the figure
# 
# # From first simulations: rho = 0.8
# # Model parameters 
# # fraud parameters
# (L = 120) #fraud level
# (rho = 0.8) #fraction of L recovered if convicted
# (phi = 40) #penalty if convicted
# #(phi = 80) #penalty if convicted
# #
# # Utility parameters
# (delta = 2) #WB discomfort 
# (gamma = 1) #utility concavity parameter
# 
# # Finding the range of compensation C with prob in [0,1]
# # pw < 1 if C <
# (c_max1 = (2*delta*(phi +rho*L)^2)/L^2)
# # pf < 1 if C > 
# (c_min1 = (2*delta*L)/(phi+rho*L))
# #
# # Derivations
# # eq (9)
# (pw1.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# # eq (10)
# (pf1.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# # also in eq (10) expected conviction prob Elambda
# (lambda_bar1.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))
# 
# # eq (11): expected fraud loss 
# (eloss1.vec = pf1.vec*(L -pw1.vec *lambda_bar1.vec *(rho*L -c.vec +phi)))
# 
# # From second simulations: rho = ?? phi=??
# # Model parameters 
# # fraud parameters
# (L = 120) #fraud level
# (rho = 0.7) #fraction of L recovered if convicted
# (phi = 40) #penalty if convicted
# #(phi = 80) #penalty if convicted
# 
# # verify assumption that ensures an interval of C with prob in [0,1]
# (1-rho)*L
# phi > (1-rho)*L
# #
# # Utility parameters
# (delta = 2) #WB discomfort 
# (gamma = 1) #utility concavity parameter
# #
# # Derivations
# # eq (9)
# (pw2.vec = ((c.vec*L^2)/((2*delta*(phi+rho*L)^2)))^(1/3) )
# # eq (10)
# (pf2.vec = ((2*delta*L)/(c.vec*(phi+rho*L)))^(1/3))
# # also in eq (10) expected conviction prob Elambda
# (lambda_bar2.vec = (((delta^2*L^2))/(2*c.vec^2*(phi+rho*L)^2))^(1/3))
# 
# # eq (11): expected fraud loss 
# (eloss2.vec = pf2.vec*(L -pw2.vec *lambda_bar2.vec *(rho*L -c.vec +phi)))
# 
# # Making it a data frame
# (eloss12.df = data.frame(c.vec, eloss1.vec, eloss2.vec))
# 
# ggplot(eloss12.df, aes(x=c.vec)) +geom_line(aes(y=eloss1.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=eloss2.vec), linetype="longdash", linewidth=1.2, color="red") 
# 
# 
# #+geom_line(aes(y=profitx), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(0,600,50)) +labs(x=TeX("Probability of reciprocal import tariffs: $\\lambda$"), y=TeX("Expected profits of firm  $X$: $E \\pi^X$, $E \\pi_H^X$, $E \\pi_F^X$")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 445, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.3, y = 520, label = TeX("$E \\pi^X=E \\pi^Y$"), size = 8, color="black") +annotate("text", x = 0.3, y = 360, label = TeX("$E \\pi_H^X =E \\pi_F^Y$"), size = 8, color="blue") +annotate("text", x = 0.3, y = 200, label = TeX("$E \\pi_F^X =E \\pi_H^Y$"), size = 8, color="red")