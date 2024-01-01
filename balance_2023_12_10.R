# balance_2023_mm_dd Why do banks require minimum balance?

# Packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

# parameters
rho = 2 # return on bank investment
eta = 120 # total number of depositors
theta = 0.5 # fraction of high-balance depositors
betal = 10 # low balance dollar amount
betah = 30# high balance dollar amount

# preparing for assumption 1
#tau needs to be higher than the expression below
rho*((1-theta)*betal + theta*betah) #

# preparing for Result 1a, eq 7 (threshold for 2 cases)
rho*betah

# Hence, simulations should run on tau seq as follows:
(tau.vec = seq(40, 80, 0.5))# horizontal axis
(dim_tau = length(tau.vec))# dimension of tau

## subsection 2.1: both set min balance (mm)
(fmm1 = tau.vec - rho*betal)
(fmm2 = tau.vec - rho*betal)
#
(profitmm1 = (eta/2)*((1-theta)*tau.vec + rho*theta*betah))
(profitmm2 = (eta/2)*((1-theta)*tau.vec + rho*theta*betah))
#
(xmml = (fmm2-fmm1+tau.vec)/(2*tau.vec))
(xmmh = rep(0.5, dim_tau))

## subsection 2.2: none sets min balance (ff)
(fff1 = tau.vec - rho*((1-theta)*betal + theta*betah))
(fff2 = tau.vec - rho*((1-theta)*betal + theta*betah))
#
(profitff1 = (eta*tau.vec/2))
(profitmm2 = (eta*tau.vec/2))
#
(xff = (fff2-fff1+tau.vec)/(2*tau.vec))

## subsection 4.1: Asymmetric: bank 1 sets min bal, bank 2 all depositors pay f2
(fmf1 = (3*tau.vec -rho*theta*betah - rho*(3-theta)*betal)/(theta+3))
(fmf2 = ((3-theta)*tau.vec -2*rho*theta*betah -3*rho*(1-theta)*betal)/(theta+3))
#
(xmfl = (fmf2-fmf1+tau.vec)/(2*tau.vec))
(xmfh = (fmf2+tau.vec)/(2*tau.vec))
#
(nmfl1 = xmfl*(1-theta)*eta)
(nmfl2 = (1-xmfl)*(1-theta)*eta)
(dmfl1 = nmfl1*betal)
(dmfl2 = nmfl2*betal)
#
(nmfh1 = xmfh*theta*eta)
(nmfh2 = (1-xmfh)*theta*eta)
(dmfh1 = nmfh1*betah)
(dmfh2 = nmfh2*betah)
#
(profitmf1 = rho*(dmfl1+dmfh1)+fmf1*nmfl1)
(profitmf2 = rho*(dmfl2+dmfh2)+fmf2*(nmfl2+nmfh2))

# prepare for drawing
(balance.df = data.frame(tau.vec, profitmm1, profitff1, profitmf1, profitmf2))
# Finding \bar\tau for the figure
(profit_diff.vec = balance.df$profitff1 - balance.df$profitmf1)
length(profit_diff.vec)
tau.vec[32]
tau.vec[33]
(tau_bar = 55.75)


# drawing (Figure 6 in the paper)
ggplot(balance.df, aes(x=tau.vec)) +geom_line(aes(y=profitmm1), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=profitff1), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=profitmf1), linetype="solid", size=1.2, color="red") +geom_line(aes(y=profitmf2), linetype="solid", size=1.2, color="blue") + scale_x_continuous(breaks = seq(40,80,10)) + scale_y_continuous(breaks = seq(2000,5000,500)) +theme(axis.text.x = element_text(size = 18, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +labs(x=TeX("Banks' market power ($\\tau$)"), y="Profits of  bank 1 and 2 (three scenarios)")  +annotate("text", x = 45, y = 3350, label =TeX("$\\pi_1^{mm}=\\pi_2^{mm}$"), size = 8, color="black") +annotate("text", x = 70, y = 4500, label =TeX("$\\pi_1^{ff}=\\pi_2^{ff}$"), size = 8, color="black") + geom_segment(aes(x = 60, y = 2300, xend = 60, yend = 4000), linetype="dotted",size=1.2) +annotate("text", x = 61, y = 4070, label =TeX("$\\tau = \\hat{\\tau} = \\rho \\beta^H"), size = 8, color="black") +annotate("text", x = 50, y = 2300, label =TeX("Moderate market power"), size = 6, color="black")+annotate("text", x = 68, y = 2300, label =TeX("Strong market power"), size = 6, color="black") +annotate("text", x = 70, y = 3700, label =TeX("$\\pi_1^{mf}$"), size = 8, color="red") +annotate("text", x = 70, y = 3100, label =TeX("$\\pi_2^{mf}$"), size = 8, color="blue") + geom_segment(aes(x = 55.75, y = 2400, xend = 55.75, yend = 3650), linetype="dotted",size=1.2) +annotate("text", x = 55.70, y = 3730, label =TeX("$\\bar{\\tau}$"), size = 8, color="black")

### Start Table 1 in the Introduction (account min balance and fees)
item.vec = c("Bank", "Account type", "Min balance ($)", "Monthly fee ($)")
#
(chase1.vec = c("Chase", "Secure Banking", NA, 4.95))
(chase2.vec = c("Chase", "Total Checking", 1500, 4.95))
(chase3.vec = c("Chase", "Premier Plus Checking", 15000, 25.00))
#
(boa1.vec = c("Bank of america", "SafeBalance", 500, 4.95))
(boa2.vec = c("Bank of america", "Advantage Plus", 1500, 12.00))
(boa3.vec = c("Bank of america", "Advantage Relationship Banking", 20000, 25))
#
(citi1.vec = c("Citibank", "Access Checking", NA, 5.00))
(citi2.vec = c("Citibank", "Regular Checking", 250, 15.00))
#
(wf1.vec = c("Wells Fargo", "Access Banking", NA, 5.00))
(wf2.vec = c("Wells Fargo", "Everyday Checking", 500, 10.00))
(wf3.vec = c("Wells Fargo", "Prime Checking", 20000, 25.00))
#
(usbank.vec = c("U.S. Bank", "Smartly Checking", 1500, 6.95))

# Turns vectors into rows of a data frame
balance1.df = data.frame()
(balance1.df = rbind(chase1.vec, chase2.vec, chase3.vec, boa1.vec, boa2.vec, boa3.vec, citi1.vec, citi2.vec, wf1.vec, wf2.vec, wf3.vec, usbank.vec))
#
(colnames(balance1.df) = item.vec)
balance1.df
#
# note that balance1.df has atomic vectors => calling x.df$col_name won't work because it was constructed with rbind of vectors. Hence,
balance2.df = as.data.frame(balance1.df)
colnames(balance2.df)
balance2.df$`Min balance ($)`
#
str(balance2.df)
#
balance2.df$`Min balance ($)` = as.numeric(balance2.df$`Min balance ($)`)
balance2.df$`Monthly fee ($)` = as.numeric(balance2.df$`Monthly fee ($)`)
#
str(balance2.df)

# adding alternative costs of maintaining min bal
# Cancelled because some balances can be linked to savings which bears some interest 
#balance3.df = balance2.df
#dim(balance3.df)
#(balance3.df$'1%' = balance3.df$`Min balance ($)`*0.01/12)
#(balance3.df$'2%' = balance3.df$`Min balance ($)`*0.02/12)
#(balance3.df$'3%' = balance3.df$`Min balance ($)`*0.03/12)
#(balance3.df$'4%' = balance3.df$`Min balance ($)`*0.04/12)
#(balance3.df$'5%' = balance3.df$`Min balance ($)`*0.05/12)

# exporting to LaTeX table
balance2.df
dim(balance2.df)
#
digitm = matrix(c(0,0,0,0,2), nrow = 12, ncol = 4+1, byrow = T)
#
print(xtable(balance2.df, digits = digitm), include.rownames = F, hline.after=c(0,3,6,8,11,12))
