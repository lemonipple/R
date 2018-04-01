admission <- read.csv("C:/Users/rgopal/Desktop/Session 4/Data/admission.csv")
GMAT = admission$GMAT
GPA = admission$GPA

# Test of correlation

# compute the test statistic
tstat = cor(GMAT,GPA)

# Describe the population based on the hypothesis, 
# draw a synthetic sample and compute the metric
f1 = function()
{
  x1 = rnorm(length(GMAT),mean = mean(GMAT),sd = sd(GMAT))
  x2 = rnorm(length(GMAT),mean = mean(GPA),sd = sd(GPA))
  return(cor(x1,x2))
}

# Create the sampling distribution
sdist = replicate(10000,f1())

# Draw the sampling distribution and compute p-value
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

# Shape test
plot(density(GMAT))
plot(density(GPA))
GPA1 = (GPA-mean(GPA))/sd(GPA)
GMAT1 = (GMAT-mean(GMAT))/sd(GMAT)
plot(density(GPA1))
lines(density(GMAT1),col="blue")

# compute the test statistic
q = c(0.1,0.2,0.3,0.67,0.96)
n1 = quantile(GPA1,probs = q)
n2 = quantile(GMAT1,probs = q)
tstat = sum(abs(n1-n2))

# Describe the population based on the hypothesis, 
# draw a synthetic sample and compute the metric
f1 = function()
{
  x1 = rnorm(length(GMAT))
  x2 = rnorm(length(GPA))
  n1 = quantile(x1,probs = q)
  n2 = quantile(x2,probs = q)
  return(sum(abs(n1-n2)))
}

# Create the sampling distribution
sdist = replicate(10000,f1())

# Draw the sampling distribution and compute p-value
plot(density(sdist))
abline(v=tstat,col="red")
length(sdist[sdist>tstat])/length(sdist)

# Confidence Intervals
f1 = function()
{
  x = rnorm(length(GMAT),mean = mean(GMAT),sd=sd(GMAT))
  return(quantile(x,probs = 0.75))
}

# Create the sampling distribution
sdist = replicate(10000,f1())

# compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))

# Non parameteric test of median

# compute the test statistic
tstat = sum(ifelse(GMAT>500,1,0))

# Describe the population based on the hypothesis, 
# draw a synthetic sample and compute the metric
f1 = function()
{
  v = c(0,1)
  p = c(0.5,0.5)
  x = sample(size = length(GMAT),x = v,prob = p,replace = T)
  return(sum(x))
}
# Create the sampling distribution
sdist = replicate(10000,f1())

# Draw the sampling distribution and compute p-value
plot(density(sdist))
abline(v=tstat,col="blue")
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue


# Bootstrapped Confidence Intervals

f1 = function()
{
  x = sample(size = length(GMAT),x=GMAT,replace = T)
  return(mean(x))
}

# Create the sampling distribution
sdist = replicate(10000,f1())

# compute confidence interval
plot(density(sdist))
quantile(sdist,probs = c(0.025,1-0.025))












