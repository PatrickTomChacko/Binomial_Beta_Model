library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
library(bayesplot)
SEED<- 1479


### Q1.Consider first the data corresponding to the survey on the local property tax. 

#We denote by $\theta_1$ the probability an individual supports this bill. We are asked to assume that $\theta_1$ follows a beta distribution and using our prior belief that 5 individuals sampled 2 would respond 'Yes', therefore I take y = 2 as the number of success and n =5 as number of participants this gives me beta(y+1,n-y+1) equal to beta(3,4) as the distribution of $\theta_1$. 


df <- data.frame(theta = seq(0.2, 0.5, 0.001))   #likely value of theta near 0.4
df_prior<- dbeta(df$theta,3,4)
plot(df$theta,df_prior, main = "Prior distribution beta(3,4)",xlab = "Theta_1", ylab = "p(θ)")


#Looking at the plot I am convinced that my prior distribution has the most likely value of 0.4 which is equal to 2/5 as given.

#Now, assuming a binomial likelihood leads to a beta posterior distribution.


#Now my posterior function is proportional to product of likelihood and prior.
#Therefore 
## $$p(θ_1|y , n, M) ∝ [\theta_1^y * (1 - \theta_1)^{(n-y)})] * [\theta_1^{\alpha -1} * (1 - \theta_1^{(\beta -1)})]\\
## = θ_1^{(y +α−1)}*(1 − θ)^{n−y +β−1}\\
## = Beta(θ_1|α + y , β + n − y )$$



y <- 32         #32 answered Yes, in the sample
n <- 100        #Total participants
alpha <- 3      #using my prior distribution beta(3,4)
beta <- 4
df$prior <- dbeta(df$theta, alpha, beta)   #prior
df$posterior <- dbeta(df$theta, alpha + y, beta + n-y)   #likelihood function

plot(df$theta,df$posterior, main = "Posterior distribution beta(35,72)",xlab = "Theta_1", ylab = "p(θ|y)")

#### Provide a 80% credible interval for θ1. 

#We can now compute a $80\%$ credible interval for the posterior distribution. To do this we set up a new sequence starting at the $10$th percentile of the $Beta(35,72)$ distribution and ending at the $90$th percentile.

df2 <- data.frame(theta = seq(qbeta(0.1,alpha+y,beta+n-y), qbeta(0.9, alpha+y,beta+n-y), length.out = 100))
# compute the posterior density
df2$posterior <- dbeta(df2$theta, alpha+y,beta+n-y)

#Let us plot our posterior distribution with 80% credible intervals
ggplot(mapping = aes(theta, posterior)) +
  geom_line(data = df) +
  # Add a layer of colorized 80% posterior interval
  geom_area(data = df2, aes(fill='1')) +
  labs(title='Beta prior -> Posterior is Beta(35,72)', y = '', x = 'theta_1') +
  scale_y_continuous(expand = c(0, 0.10), breaks = NULL) +
  scale_fill_manual(values = 'lightgreen', labels = '80% posterior interval') +
  theme(legend.position = 'bottom', legend.title = element_blank())


#Here are $10\%$ and $90\%$ percentiles which correspond to the $80\%$ credible interval:


 c(qbeta(0.1,alpha+y,beta+n-y), qbeta(0.9,alpha+y,beta+n-y))

#80% credible interval for $\theta_1$ is [0.27,0.39]


#### Compare this credible interval to one based on a Monte Carlo approximation using a Monte Carlo sample of size 200.


#Monte Carlo sampling with 200 samples

#Let us take 200 samples from posterior distribution and store it in a vector 'sample'.

sample<-rbeta(200,alpha+y,beta+n-y)
sum(sample)/200  #This is our Monte Carlo approximation of theta_1 received after sampling from posterior.

Our Monte Carlo estimate of $\theta_1$ comes out to be near 0,33 in point estimate form. The value keeps on changing while sampling differently.

ggplot() +
  geom_density(aes(sample)) + 
  labs(x="sampled theta_1")+
  labs(title = 'Probability Density of theta_1 from MC' )



library(HDInterval)
HDInterval::hdi(sample, credMass=0.80)[1:2]

#Now finding a credible interval via Monte Carlo sampling we get the 80% credible interval to be near [0.27,0.38]

#We can see that the credible intervals using both inferences yield very similar intervals for the value of $\theta_1$ to lie in. Therefore we can confirm that our modeling is accurate along with the equations.

#### Comment briefly on this credible interval.

#80% credible interval
#In Bayesian analysis since $\theta_1$ is considered a random variable it makes sense to say that with 80% probability that our parameter($\theta_1$) will take up a value in the given bounds of our 80% credible interval.

### Q2. Again, consider the data corresponding to the survey on the local property tax. Use Monte Carlo methods to approximate a 90% credible interval for the odds for θ1. 

#It is of interest to explore the odd ratio of the probability of an individual who supports the bill, therefore we compute 

 # $$ \phi = \frac{(1-\theta_1)}{\theta_1}$$

phi <- (1-sample)/sample    # R suppors vector arithmetics

ggplot() +
  geom_density(aes(phi))+
  labs(x = "Odds ratio")+labs(title = "Odds for theta1")

HDInterval::hdi(phi, credMass=0.90)[1:2]

#### Comment briefly on this 90% credible interval.

#Odds ratio is a measure of degree of association between two events.$(1-\theta_1)$ corresponds to the probability that an individual doesen't support the bill because $\theta_1$ corresponds to the probability of supporting a bill, therefore these are our two events.
#So we get a 90% credible interval of odds ratio for theta_1 to be close to [1.37,2.78]. 
#This would mean that with 90% probability our odds ratio will lie in the given credible interval [1.37,2.78], this would further imply that the odds of 1- $\theta_1$ happening is half to that of $\theta_1$ happening, or the probability an individual supports the bill is nearly twice as compared to not supporting the bill.


### Q3. The polling company would like to assess if there is more support to decriminalise cannabis than for the introduction of a local property tax. To do this he would like to assess the odds ratio between θ1 and θ2, defined as odds.ratio = [θ1/(1 − θ1)]/[θ2/(1 − θ2)]. 

#Let $\theta_2$ be the probabilty that an individual supports cannabis use, we have the same prior distribution for $\theta_2$ but here our binomial Model M has different arguments. Number of success = y = 61 and number of observations = 150

df_theta2 <- data.frame(theta = seq(0.2, 0.5, 0.001)) #2/5 = 0.4 is my theta of interest 
y_2 <- 61         #61 suport decriminalising cannabis use
n_2 <- 150
alpha <- 3      #using my prior distribution beta(3,4)
beta <- 4
df$prior <- dbeta(df$theta, alpha, beta)   #same prior
df$posterior_2 <- dbeta(df$theta, alpha + y_2, beta + n_2-y_2)   #likelihood function


plot(df$theta,df$posterior_2, main = "Posterior distribution beta(64,93)",xlab = "Theta_2", ylab = "p(θ|y)")

#### Estimate a 90% credible interval for the posterior odds ratio using Monte Carlo methods based on a sample size of 200.

sample_2<-rbeta(200, alpha + y_2, beta + n_2-y_2)  #Monte Carlo Sampling 
phi2 <- (1-sample_2)/sample_2
odds_ratio <- phi/phi2  #odds ratio of theta_1/theta_2

ggplot() +
  geom_density(aes(odds_ratio))+
  labs(x = "Odds ratio")+
  labs(title = 'Odds ratio of theta_1,theta_2')

HDInterval::hdi(odds_ratio, credMass=0.90)[1:2]

#### Provide a clear interpretation of this credible interval.
#The 90% credible interval for odds ratio is near [0.8,1.9] and the peak from the graph seems to occur approximately at 1.25, therefore since this is our credible interval and not a confidence interval we can say that the probability that our true parameter lies in the given range is 90%.

#Here it means that the probability of phi occuring is 1.25 times of phi2 occuring, implying that the probability that an indivdual supports the local property tax is 1.25 times greater than to the probability an individual would support to decriminalise cannabis, to a credibility of 90%.

#(since probability lies between 0 and 1 it cannot be lesser and has to be greater.)


### Q4. Write an expression for the posterior probability that θ1 > θ2. Use Monte Carlo methods to estimate this probability. 

#To compare this quantity I am going to take 500 MC samples from each of the posterior distribution of interest and I am going to divide the samples of θ1 by θ2 and plot them to see how different is the vertical intercept from 1. 

#If different we can then find the probability by dividing Total Success by Total Observations.


theta1_pos = rbeta(500,35,72)
theta2_pos = rbeta(500,64,93)
theta_ratio = theta1_pos/theta2_pos
ggplot() +
  geom_vline(xintercept = 1, linetype='dotted')+
  geom_density(aes(theta_ratio))+
  labs(x = "Theta ratio")+
  labs(title = 'ratio of theta_1,theta_2')

#The peak seems to be around 0.8 approximately, which is similar to saying that theta2 is greater than theta1. 

#The probability that theta1>theta2 is the number of cases in which theta1/theta2 > 1 divide by 500 (our sample size)


theta_ratio_df <- as.data.frame(theta_ratio)
#theta_ratio_df
sum(theta_ratio_df>1)/nrow(theta_ratio_df) #Monte carlo

#### Comment on the estimated probability.
#We get the probability as 0.088, thus we can say that the probability of theta1 being greater than theta2 is 0.088 or the probability an individual supports local property tax being greater than an individual supporting decriminalising of cannabis is 8.8% which means not very likely.

### Q5. Write a Stan program to carry out the same analysis in 3. Briefly present your analysis and compare to your answer in question 3.



writeLines(readLines("Q5_.stan"))

dbin <- list(N1 = 100 ,N2 = 150, y1 = 32, y2 = 61 )
fit_model <- stan(file = 'Q5_.stan', data = dbin, seed = SEED, iter = 1000)

monitor(fit_model, probs = c(0.1, 0.9))

draws <- as.data.frame(fit_model)
mcmc_hist(draws, pars = 'oddsratio') +
  geom_vline(xintercept = 1) +
  labs(title = "Odds ratio of theta1 to theta 2")+
  scale_x_continuous(breaks = c(seq(0.25,3.75,by=0.25)))

#The generated odds ratio in Q3 has a 90% credible interval of [0.91,2.06] and the 90% credible interval in Q5 is given at [0.9,2.2]. We can see the odds ratio using different programs have approximately equal intervals.

#Note - The odds ratio interpretation depends on the formula as we take the reciprocal to be used in our generated quantities block.

#The peak seems to occur around 1.25, which gives us the similar result as received in Q3.

#The programming via stan is much more easier to compute the same result as obtained in Q3 using R.

#We get BULK_ESS and TAIL_ESS are mesasures of effective sample size, we have it to be more than 1000 which is good as we are looking to get a value > 100.

#4 chains with 1000 iterations each has been carried, sampling 4000 times with 4 different starting points, and carrying the process iteratively.

#R_hat = 1 and we are looking for a value < 1.05 to observe convergence.

#Mean and Sd have their usual interpretation.

#lp__, is the logarithm of the (unnormalized) posterior density as calculated by Stan.

#Acknowledgemnts :- Nial Friel (for my rmd I closely referred tutorial posted), R help packages
