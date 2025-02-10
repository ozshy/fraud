# wb_fin_2025_x_y.R 
# Simulations (verification) of computations in a paper titled: "Whistleblowers and Financial Fraud."

# packages used
library(ggplot2); theme_set(theme_bw())
#library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables
(lambda = 0.5)# 50% recovery of the loss
(L = 120)# Loss cause by the fraud
(gamma = 12)# parameter in the prob function

(c.vec = seq(0, 1, 0.001))# compensation fractions
#total compensation as function of fraction c
(C.vec = c.vec*lambda*L)# 
plot(C.vec)

# probability as function of fraction c
(p.vec = C.vec/(gamma + C.vec))
plot(p.vec, type="l")

# optimal compensation fraction
(c_star = (sqrt(gamma)*sqrt(gamma+lambda*L) -gamma)/(lambda*L))

# Result 1: how c_star varies with lambda*L
(lambda.vec = seq(0, 1, 0.001))
#
(c_star.vec = (sqrt(gamma)*sqrt(gamma+lambda.vec*L) -gamma)/(lambda.vec*L))
plot(c_star.vec, type="l")

# Result 2b
gamma
(gamma.vec = seq(1:24))
(c_star_gamma.vec = (sqrt(gamma.vec)*sqrt(gamma.vec+lambda*L) -gamma.vec)/(lambda*L))

# eq (5) in paper, equilibrium total compensation and p. Result 3a:
(C_star_lambda.vec = c_star.vec *lambda.vec *L)
plot(C_star_lambda.vec, type="l")

# eq (5) in paper and Result 3b:
(p_lambda.vec = C_star_lambda.vec/(gamma + C_star_lambda.vec))
plot(p_lambda.vec, type="l")

### Section 4: Incentives
gamma = 0.2
lambda = 0.8
# need to express all as functions of L
(F = 2)# fine/penalty
(J = 2)# jail time in monetary units
#
(L.vec = seq(0,12,0.01))
#
(p_bar.vec = L.vec/(lambda*L.vec +F +J))
plot(p_bar.vec, type="l")

# recall p^*
(p_star.vec = 1 -sqrt(gamma/(gamma +lambda*L.vec)))
plot(p_star.vec, type="l")

#build a data frame. Use 3 values of F+J
(F=0.1); (J=0.1)
(p_bar_low.vec = L.vec/(lambda*L.vec +F +J))
plot(p_bar_low.vec, type="l")
#
(F=1); (J=1)
(p_bar_medium.vec = L.vec/(lambda*L.vec +F +J))
plot(p_bar_medium.vec, type="l")
#
(F=2); (J=2)
(p_bar_high.vec = L.vec/(lambda*L.vec +F +J))
plot(p_bar_high.vec, type="l")

# make it a data frame
(p.df = data.frame(L.vec, p_star.vec, p_bar_low.vec, p_bar_medium.vec, p_bar_high.vec))

ggplot(p.df, aes(x=L.vec)) +geom_line(aes(y=p_star.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=p_bar_low.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=p_bar_medium.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=p_bar_high.vec), linetype="solid", size=1.2, color="magenta")

# graph from earlier versions, no longer applies
#ggplot(p.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=qaI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qbI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qaII.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(y=qbII.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.6,3.3,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium producer and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.225, y = 1.70, label =TeX("$p_B^I$"), size = 8, color="black") +annotate("text", x = 0.24, y = 2.08, label =TeX("$q_B^I$"), size = 8, color="blue") +annotate("text", x = 0.225, y = 1.90, label =TeX("$p_B^{II}$"), size = 8, color="red")  +annotate("text", x = 0.24, y = 2.39, label =TeX("$q_B^{II}$"), size = 8, color="magenta") +annotate("text", x = 0.20, y = 2.20, label =TeX("$p_A^I$"), size = 8, color="black") +annotate("text", x = 0.20, y = 2.54, label =TeX("$p_A^{II}$"), size = 8, color="red") +annotate("text", x = 0.15, y = 2.80, label =TeX("$q_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 3.13, label =TeX("$q_A^{II}$"), size = 8, color="magenta") +annotate("text", x = -0.003, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.67, label =TeX("A"), size = 8, color="black")  


################
################