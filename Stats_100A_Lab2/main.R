#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Austin Vaday
# Stats 100A
# Discussion 1B
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# B I N O M I A L   D I S T R I B U T I O N S #
#***********************#
#          Q1           #
#***********************#
# 1(A) : n=10, p=0.05
  
  numTrials = 10;
  numTrialsRange = 0:numTrials;
  probability = 0.05;
  resultDistribution = dbinom(numTrialsRange, size=numTrials, prob=probability);
  title = capture.output(cat("The Binomial Distribution for n =",numTrials,", p =",probability));
  plot (numTrialsRange, resultDistribution, main=title, type="b", 
          xlab="The n-th trial", ylab="Probability that the n-th trial occurs")
  
# 1(B) : n=10, p=0.50
  
  numTrials = 10;
  numTrialsRange = 0:numTrials;
  probability = 0.50;
  resultDistribution = dbinom(numTrialsRange, size=numTrials, prob=probability);
  title = capture.output(cat("The Binomial Distribution for n =",numTrials,", p =",probability));
  plot (numTrialsRange, resultDistribution, main=title, type="b", 
        xlab="The n-th trial", ylab="Probability that the n-th trial occurs")
  
# 1(C) : n=10, p=0.85
  
  numTrials = 10;
  numTrialsRange = 0:numTrials;
  probability = 0.85;
  resultDistribution = dbinom(numTrialsRange, size=numTrials, prob=probability);
  title = capture.output(cat("The Binomial Distribution for n =",numTrials,", p =",probability));
  plot (numTrialsRange, resultDistribution, main=title, type="b", 
        xlab="The n-th trial", ylab="Probability that the n-th trial occurs")
  
# 1(D) : n=50, p=0.85
  
  numTrials = 50;
  numTrialsRange = 0:numTrials;
  probability = 0.85;
  resultDistribution = dbinom(numTrialsRange, size=numTrials, prob=probability);
  title = capture.output(cat("The Binomial Distribution for n =",numTrials,", p =",probability));
  plot (numTrialsRange, resultDistribution, main=title, type="b", 
        xlab="The n-th trial", ylab="Probability that the n-th trial occurs")
  
# 1(E) : n=200, p=0.85
  
  numTrials = 200;
  numTrialsRange = 0:numTrials;
  probability = 0.85;
  resultDistribution = dbinom(numTrialsRange, size=numTrials, prob=probability);
  title = capture.output(cat("The Binomial Distribution for n =",numTrials,", p =",probability));
  plot (numTrialsRange, resultDistribution, main=title, type="b", 
        xlab="The n-th trial", ylab="Probability that the n-th trial occurs")
  
#***********************#
#          Q2           #
#***********************#
  # When we change the probability of success (increased p for a constant value of n), we see that
  # the graph goes from skewed-right distribution to symmetric distribution and finally to skewed-left
  # distribution. Meaning that as the probability gets larger for a constant number of trials,
  # the center value becomes larger and larger (it shifts to the right).
 
  # When we change the number of trials (increased n for a constant value of p), we see that
  # the graph shape remains roughly the same, with the center/peak always located at .85*n.

#***********************#
#          Q3           #
#***********************#
  
  # (A) : n=10, p=0.05
  
  numTrials = 10;
  probability = 0.05;
  randomSample = rbinom(10000, size=numTrials, prob=probability);
  title = capture.output(cat("The Random Binomial Distribution Histogram for n =",numTrials,", p =",probability));
  hist (randomSample, main=title);
  abline(v=mean(randomSample),col="red");

  # (B) : n=10, p=0.50
  
  numTrials = 10;
  probability = 0.50;
  randomSample = rbinom(10000, size=numTrials, prob=probability);
  title = capture.output(cat("The Random Binomial Distribution Histogram for n =",numTrials,", p =",probability));
  hist (randomSample, main=title);
  abline(v=mean(randomSample),col="red");
  
  # (C) : n=10, p=0.85
  
  numTrials = 10;
  probability = 0.85;
  randomSample = rbinom(10000, size=numTrials, prob=probability);
  title = capture.output(cat("The Random Binomial Distribution Histogram for n =",numTrials,", p =",probability));
  hist (randomSample, main=title);
  abline(v=mean(randomSample),col="red");
  
  # (D) : n=50, p=0.85
  
  numTrials = 50;
  probability = 0.85;
  randomSample = rbinom(10000, size=numTrials, prob=probability);
  title = capture.output(cat("The Random Binomial Distribution Histogram for n =",numTrials,", p =",probability));
  hist (randomSample, main=title);
  abline(v=mean(randomSample),col="red");
  
  # (E) : n=200, p=0.85
  
  numTrials = 200;
  probability = 0.85;
  randomSample = rbinom(10000, size=numTrials, prob=probability);
  title = capture.output(cat("The Random Binomial Distribution Histogram for n =",numTrials,", p =",probability));
  hist (randomSample, main=title);
  abline(v=mean(randomSample),col="red")
  
# H Y P E R G E O M E T R I C   D I S T R I B U T I O N S #
  
#***********************#
#       QUESTION        #
#***********************#
  # We see that the parameters of the hypergeometric distribution have been changed as follows:
  #   m, the number of girls in the orphange, reduces from 46 to 10
  #   n, the number of boys  in the orphange, reduces from 49 to 6
  #   r, the number of children adopted,      reduces from 20 to 4
  # With this in mind, we notice a few things:
  #   1) The probability of occurance for each discrete value (0 to 4),is on average, higher than
  #       that of the original. (A smaller range)
  #   2) The plot is no longer near-symmetric, but now is skewed-left. 
  #   3) The sum of all the probabilities still adds up to 1.
  #   4) We are more likely to have a greater amount of girls selected for the 
  #       latter distribution because of how we hnow have more girls (10) than boys (6).
  
# G E O M E T R I C   D I S T R I B U T I O N S #
#***********************#
#          Q1           #
#***********************#
  probability = 0.15;
  numSize=1000;
  randomSample = rgeom(numSize, prob=probability) + 1; #Add 1 to account for notation differences
  
  # (A): Find Mean and Variance, 1st way using formula. 2nd way using random sample
  # 1st way:
    myMean = 1 / probability; 
    myMean;
    myVariance = (1 - probability)/(probability*probability);
    myVariance;
  
  # 2nd way:
    mean(randomSample);
    var(randomSample);

  # (B)
    title = capture.output(cat("Random Geometric Distribution Histogram for p =",probability));
    #plot (randomSample, type="h", main=title, lwd=2, 
    #     xlab="First Success", ylab="Probability that this occurs")
    hist(randomSample, main=title);
    summary(randomSample);
  
#***********************#
#          Q2           #
#***********************#
  probability = 0.35;
  numSize=1000;
  randomSample = rgeom(numSize, prob=probability) + 1; #Add 1 to account for notation differences
  title = capture.output(cat("Random Geometric Distribution Histogram for p =",probability));
  hist(randomSample, main=title);
  summary(randomSample);
  
  probability = 0.55;
  numSize=1000;
  randomSample = rgeom(numSize, prob=probability) + 1; #Add 1 to account for notation differences
  title = capture.output(cat("Random Geometric Distribution Histogram for p =",probability));
  hist(randomSample, main=title);
  summary(randomSample);
  
  probability = 0.85;
  numSize=1000;
  randomSample = rgeom(numSize, prob=probability) + 1; #Add 1 to account for notation differences
  title = capture.output(cat("Random Geometric Distribution Histogram for p =",probability));
  hist(randomSample, main=title);
  summary(randomSample);
  
  # We note that as we increase the probability of success, the mean approaches 1. This means that
  # we have increasingly higher chances of getting a success on the first few trials. In the histograms,
  # we also notice that the frequency of the lowest numbers tend to increase, while
  # the numbers beyond tend to decrease in frequency. If the probability is low, you are required
  # to conduct more trials 
  
#***********************#
#          Q3           #
#***********************#
# P O I S S O N   D I S T R I B U T I O N
  
# Repeating Q1 & Q2 but with a Poisson distribution, lambda = 3:
  lambda = 3;
  numSize=1000;
  randomSample = rpois(numSize, lambda);
  
  # (A): Find Mean and Variance, 1st way using formula. 2nd way using random sample
  # 1st way:
  myMean = lambda;
  myMean;
  myVariance = lambda; 
  myVariance;
  
  # 2nd way:
  mean(randomSample);
  var(randomSample);
  
  # (B):
  title = capture.output(cat("Random Poisson Distribution Histogram for lambda =",lambda));
  hist(randomSample, main=title);
  summary(randomSample);
  
  # QUESTION 2 : Increasing lambda
  
  # We know that lambda = n*p, so p = lambda/n. If we change lambda, we change p
  # (ASSUMING THAT n IS CONSTANT). So,
  # let's increase lambda.
  lambda = 10;
  randomSample = rpois(numSize, lambda);
  title = capture.output(cat("Random Poisson Distribution Histogram for lambda =",lambda));
  hist(randomSample, main=title);
  summary(randomSample);
  
  lambda = 30;
  randomSample = rpois(numSize, lambda);
  title = capture.output(cat("Random Poisson Distribution Histogram for lambda =",lambda));
  hist(randomSample, main=title);
  summary(randomSample);
  
  lambda = 70;
  randomSample = rpois(numSize, lambda);
  title = capture.output(cat("Random Poisson Distribution Histogram for lambda =",lambda));
  hist(randomSample, main=title);
  summary(randomSample);
  
  lambda = 200;
  randomSample = rpois(numSize, lambda);
  title = capture.output(cat("Random Poisson Distribution Histogram for lambda =",lambda));
  hist(randomSample, main=title);
  summary(randomSample);
  
  # In the case of a Poisson distribution, we see that altering lambda (and the probability)
  # causes the mean to become whatever value lamda is. Thus, the graph will be shifted accordingly
  # to make sure that the mean is equal to lamda.
  
# N E G A T I V E   B I N O M I A L   D I S T R I B U T I O N
  
  # Repeating Q1 & Q2 but with a Negative Binomial Distribution:
  numSuccessfulTrials = 3;
  probability = 0.15;
  numSize = 10000;
  randomSample = rnbinom(numSize, size=numSuccessfulTrials, prob=probability) + numSuccessfulTrials;
  title = capture.output(cat("Random Negative Binomial Distribution Histogram for n =",numSuccessfulTrials,", p =",probability));
  hist (randomSample, main=title);
  summary(randomSample);

  # (A): Find Mean and Variance, 1st way using formula. 2nd way using random sample
  # 1st way:
  myMean = numSuccessfulTrials / probability;
  myMean;
  myVariance = numSuccessfulTrials * (1 - probability) / (probability * probability); 
  myVariance;
  
  # 2nd way:
  mean(randomSample);
  var(randomSample);

  # (B): Increasing probabilities
  probability = 0.35;
  randomSample = rnbinom(numSize, size=numSuccessfulTrials, prob=probability) + numSuccessfulTrials;
  title = capture.output(cat("Random Negative Binomial Distribution Histogram for n =",numSuccessfulTrials,", p =",probability));
  hist (randomSample, main=title);
  summary(randomSample);
  
  probability = 0.55;
  randomSample = rnbinom(numSize, size=numSuccessfulTrials, prob=probability) + numSuccessfulTrials;
  title = capture.output(cat("Random Negative Binomial Distribution Histogram for n =",numSuccessfulTrials,", p =",probability));
  hist (randomSample, main=title);
  summary(randomSample);
  
  probability = 0.75;
  randomSample = rnbinom(numSize, size=numSuccessfulTrials, prob=probability) + numSuccessfulTrials;
  title = capture.output(cat("Random Negative Binomial Distribution Histogram for n =",numSuccessfulTrials,", p =",probability));
  hist (randomSample, main=title);
  summary(randomSample);
  
  probability = 0.95;
  randomSample = rnbinom(numSize, size=numSuccessfulTrials, prob=probability) + numSuccessfulTrials;
  title = capture.output(cat("Random Negative Binomial Distribution Histogram for n =",numSuccessfulTrials,", p =",probability));
  hist (randomSample, main=title);
  summary(randomSample);

  # When we increase the probability of success, we see that the frequency of 
  # r = 3 becomes increasingly larger. We also see that the mean shifts to the
  # left, and approaches 3 as the probability increases. This is because if the
  # probability is so high, we would only need to "flip a coin" 3 times for
  # 3 succeses