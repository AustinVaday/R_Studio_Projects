#:::::::::::::::::::::::NORMAL DISTRIBUTION::::::::::::::::::::::::::::::::::::#
# ******** THE PROBABILITY DENSITY FUNCTION ********:

x= seq(-4, 4, length=200)
y= 1/sqrt(2*pi)*exp(-x^2/2)
plot(x, y, type="l", lwd=2, col="red") 

# is the same as:

x = seq(-4, 4, length=200);
y = dnorm(x, mean=0, sd=1);
plot(x, y, type="l", lwd=2, col="red");

# Q: Check the effect of changing the mean and then the standard 
# deviation and notice the effects onthe normal distribution plots. 
# What kind of parameters are these?

# Increasing mean:
for (i in 1:5)
{
  x = seq(-4, 4, length=200);
    y = dnorm(x, mean=i, sd=1);
	  plot(x, y, type="l", lwd=2, col="red");
	  }

	  # Increasing Standard Deviation:
	  i = 0;
	  for (i in 1:5)
	  {
	    x = seq(-4, 4, length=200);
		  y = dnorm(x, mean=0, sd=i);
		    plot(x, y, type="l", lwd=2, col="red");
			}

			# Increasing Mean & Standard Deviation:
			i = 0;
			for (i in 1:5)
			{
			  x = seq(-4, 4, length=200);
			    y = dnorm(x, mean=i, sd=i);
				  plot(x, y, type="l", lwd=2, col="red");
				  }

				  # A: It is obvious that changing the mean will result in a shift of the graph,
				  # and changing the standard deviation will result in a scale of the graph.
				  # Changing both the mean and standard deviation will result in a shift and scale.
				  # The mean is the location parameter, and the variance is the shape parameter.

				  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:

				  # 1 SD From mean:
				  x=seq(-4,4,length=200)
				  y=dnorm(x)
				  plot(x,y,type="l", lwd=2, col="blue")
				  x=seq(-1,1,length=100)
				  y=dnorm(x)
				  polygon(c(-1,x,1),c(0,y,0),col="gray") 

				  pnorm(1,mean=0,sd=1)-pnorm(-1,mean=0,sd=1) 

				  # 2 SD From mean:
				  x=seq(-4,4,length=200)
				  y=dnorm(x)
				  plot(x,y,type="l", lwd=2, col="blue")
				  x=seq(-2,2,length=100)
				  y=dnorm(x)
				  polygon(c(-2,x,2),c(0,y,0),col="gray") 

				  pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1) 

				  # 3 SD From mean:
				  x=seq(-4,4,length=200)
				  y=dnorm(x)
				  plot(x,y,type="l", lwd=2, col="blue")
				  x=seq(-3,3,length=100)
				  y=dnorm(x)
				  polygon(c(-3,x,3),c(0,y,0),col="gray") 

				  pnorm(3,mean=0,sd=1)-pnorm(-3,mean=0,sd=1) 

				  # ******** TESTING QUANTILES ********:
				  qnorm(0.95, mean=0, sd=1)
				  #Hence, there is a 95% probability that a random number less than or equal to
				  # 1.645 is chosen from the standard normal distribution. 

				  qnorm(0.20, mean=0, sd=1)
				  # We now know that the probability of selecting a number from the standard 
				  # normal distribution that is greater than or equal to -0.842 is 0.80. 

				  # ******** DRAWING A RANDOM SAMPLE ********:
				  sample1 = rnorm(5000,10,4)
				  summary(sample1)
				  var(sample1)

				  # Q: Do these statistics match the parameters?
				  # A: Yes. We see that the mean is around 9.911, which is close to 10. We also see
				  # that sigma^2 = variance is around 16.16, which is close to sigma = 4.

				  #:::::::::::::::::::::::END NORMAL DISTRIBUTION:::::::::::::::::::::::::::::::::#

				  # PART (A):
				  #:::::::::::::::::::::::1-GAMMA DISTRIBUTION::::::::::::::::::::::::::::::::::::#
				  # ******** THE PROBABILITY DENSITY FUNCTION ********:

				  x= seq(0, 10, length=200)
				  alpha = 5; 
				  lamda = 2;
				  y= (x^(alpha-1)*exp(-lamda*x)*(lamda)^alpha / gamma(alpha));
				  plot(x, y, type="l", lwd=2, col="red") 

				  # is the same as:

				  x = seq(0, 10, length=200);
				  y=dgamma(x,5,2) 
				  plot(x, y, type="l", lwd=2, col="red");

				  # Q: Check the effect of changing the two parameters and notice the effects on
				  # the gamma distribution plot.
				  # What kind of parameters are these?

				  # Increasing alpha:
				  for (i in seq(5,25,5))
				  {
				    x = seq(0, 10, length=200);
					  y=dgamma(x,i,2) 
					    plot(x, y, type="l", lwd=2, col="red");
						}

						# Increasing lamda = 1/beta:
						i = 0;
						for (i in seq(2,30,5))
						{
						  x = seq(0, 10, length=200);
						    y=dgamma(x,5,i) 
							  plot(x, y, type="l", lwd=2, col="red");
							  }

							  # A: It is obvious that changing alpha will result in a shift of the graph
							  # (and a slight scale). Changing lamda will result in a scale of the graph
							  # (and a slight shift).
							  # Alpha is the location parameter, and beta is the shape parameter.

							  # ******** TESTING QUANTILES ********:
							  qgamma(0.95, 5, 2)
							  #Hence, there is a 95% probability that a random number less than or equal to
							  # 4.577 is chosen from the gamma distribution. 

							  qgamma(0.20, 5,2)
							  # We now know that the probability of selecting a number from the 
							  # gamma distribution that is greater than or equal to 1.545 is 0.80. 

							  #:::::::::::::::::::::::END 1-GAMMA DISTRIBUTION:::::::::::::::::::::::::::::::::#

							  #:::::::::::::::::::::::1-EXPONENTIAL DISTRIBUTION::::::::::::::::::::::::::::::::::::#
							  # ******** THE PROBABILITY DENSITY FUNCTION ********:

							  # lamda = 2 
							  x = seq(0, 10, length=200);
							  y=dexp(x,2) 
							  plot(x, y, type="l", lwd=2, col="red");

							  # Q: Check the effect of changing the parameter and notice the effects on
							  # the exponential distribution plot.
							  # What kind of parameters are these?

							  # Increasing lamda = 1/beta:
							  i = 0;
							  for (i in seq(2,30,5))
							  {
							    x = seq(0, 10, length=200);
								  y=dexp(x,i) 
								    plot(x, y, type="l", lwd=2, col="red");
									}

									# A: It is obvious that we cannot change alpha (no shift)
									# But changing lamda will result in a scale of the graph (increase lambda, the graph becomes steeper)
									# Thus lambda is the shape parameter.

									# ******** TESTING QUANTILES ********:
									qexp(0.95, 2)
									#Hence, there is a 95% probability that a random number less than or equal to
									# 1.498 is chosen from this distribution. 

									qexp(0.20,2)
									# We now know that the probability of selecting a number from this
									# distribution that is greater than or equal to .112 is 0.80. 

									#:::::::::::::::::::::::END 1-EXPONENTIAL DISTRIBUTION:::::::::::::::::::::::::::::::::#

									#:::::::::::::::::::::::1-CHI-SQUARED DISTRIBUTION::::::::::::::::::::::::::::::::::::#
									# ******** THE PROBABILITY DENSITY FUNCTION ********:

									# x= seq(0, 10, length=200)
									# alpha = 5; 
									# lamda = 2;
									# y= (x^(alpha-1)*exp(-lamda*x)*(lamda)^alpha / gamma(alpha));
									# plot(x, y, type="l", lwd=2, col="red") 
									# 
									# # is the same as:

									x = seq(0, 10, length=200);
									y=dchisq(x, df=1) 
									plot(x, y, type="l", lwd=2, col="red");

									# Q: Check the effect of changing the parameter and notice the effects on
									# the chi-squared distribution plot.
									# What kind of parameters are these?

									# Increasing df (degrees of freedom):
									for (i in seq(1,25,5))
									{
									  x = seq(0, 10, length=200);
									    y=dchisq(x, df=i) 
										  plot(x, y, type="l", lwd=2, col="red");
										  }

										  # A: Increasing the degrees of freedom will cause both a scale and a shift. It is a location
										  # and scale parameter. 

										  # ******** TESTING QUANTILES ********:
										  qchisq(0.95, 1)
										  #Hence, there is a 95% probability that a random number less than or equal to
										  # 3.841 is chosen from the standard normal distribution. 

										  qchisq(0.20, 1)
										  # We now know that the probability of selecting a number from the standard 
										  # normal distribution that is greater than or equal to 0.0642 is 0.80. 

										  #:::::::::::::::::::::::END 1-CHI-SQUARED DISTRIBUTION:::::::::::::::::::::::::::::::::#

										  #:::::::::::::::::::::::1-BETA DISTRIBUTION::::::::::::::::::::::::::::::::::::#
										  # ******** THE PROBABILITY DENSITY FUNCTION ********:

										  x=seq(0,1,length=200)
										  y=dbeta(x,3,3)
										  plot(x,y,type="l",lwd=2,col="blue")

										  # Q: Check the effect of changing the two parameters and notice the effects on
										  # this distribution plot.
										  # What kind  of parameters are these?

										  # Increasing shape1:
										  i=0
										  for (i in seq(3,10,1))
										  {
										    x=seq(0,1,length=200)
											  y=dbeta(x,i,3)
											    plot(x,y,type="l",lwd=2,col="blue")
												}

												# Increasing shape2:
												i=0
												for (i in seq(3,10,1))
												{
												  x=seq(0,1,length=200)
												    y=dbeta(x,3,i)
													  plot(x,y,type="l",lwd=2,col="blue")
													  }


													  # A: It is obvious that increasing shape1 causes a skewed-left graph and 
													  # increasing shape2 causes a skewed-right graph. They both shift and scale.
													  # Shape1 is the skewed-left location/shape parameter, and beta is the 
													  # skewed-right location/shape parameter.

													  # ******** TESTING QUANTILES ********:
													  qbeta(0.95,3,3)
													  #Hence, there is a 95% probability that a random number less than or equal to
													  # .811 is chosen from this distribution. 

													  qbeta(0.20,3,3)
													  # We now know that the probability of selecting a number from this 
													  #  distribution that is greater than or equal to .327 is 0.80. 

													  #:::::::::::::::::::::::END 2-BETA DISTRIBUTION:::::::::::::::::::::::::::::::::#

													  #:::::::::::::::::::::::2-UNIFORM DISTRIBUTION::::::::::::::::::::::::::::::::::::#
													  # ******** THE PROBABILITY DENSITY FUNCTION ********:

													  x=seq(0,1,length=200)
													  y=dunif(x, 0, 1)
													  plot(x,y,type="l",lwd=2,col="blue")

													  # Q: Check the effect of changing the two parameters and notice the effects on
													  # this distribution plot.
													  # What kind  of parameters are these?

													  # Increasing min:
													  i=0
													  for (i in seq(-25,0,5))
													  {
													    x=seq(0,1,length=200)
														  y=dunif(x, i, 1)
														    plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
															}

															# Increasing max:
															i=0
															for (i in seq(1,25,5))
															{
															  x=seq(0,1,length=200)
															    y=dunif(x, 0, i)
																  plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
																  }


																  # A: It is obvious that increasing our min causes an enlarged scale, while  
																  # increasing our max causes a decreased scale. The graph remains between 0 and 1.
																  # Thus, the min and max are both scale parameters.

																  # ******** TESTING QUANTILES ********:
																  qunif(0.95,0,1)
																  #Hence, there is a 95% probability that a random number less than or equal to
																  # 0.95 is chosen from this distribution. 

																  qbeta(0.20,0,1)
																  # We now know that the probability of selecting a number from this 
																  #  distribution that is greater than or equal to 0 is 0.80. 

																  #:::::::::::::::::::::::END 2-UNIFORM DISTRIBUTION:::::::::::::::::::::::::::::::::#

																  #:::::::::::::::::::::::3-CAUCHY DISTRIBUTION::::::::::::::::::::::::::::::::::::#
																  # ******** THE PROBABILITY DENSITY FUNCTION ********:

																  x=seq(0,10,length=200)
																  y=dcauchy(x,100, 1)
																  plot(x,y,type="l",lwd=2,col="blue")

																  x=seq(0,10,length=200)
																  y=dcauchy(x,1/2, 1)
																  plot(x,y,type="l",lwd=2,col="blue")

																  # Q: Check the effect of changing the two parameters and notice the effects on
																  # this distribution plot.
																  # What kind  of parameters are these?

																  # Increasing first parameter:
																  i=0
																  for (i in seq(1/2,5,1))
																  {
																    x=seq(0,10,length=200)
																	  y=dcauchy(x,i, 1)
																	    plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
																		}

																		# Increasing second parameter:
																		i=0
																		for (i in seq(1,5,1))
																		{
																		  x=seq(0,10,length=200)
																		    y=dcauchy(x,1/2, i)
																			  plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
																			  }


																			  # A: It is obvious that increasing our first parameter causes shift, while  
																			  # increasing our second parameter causes a scale.
																			  # Thus, the first parameter is a location parameter, while our second
																			  # parameter is a scale parameter

																			  # ******** TESTING QUANTILES ********:
																			  qcauchy(0.95,100)
																			  #Hence, there is a 95% probability that a random number less than or equal to
																			  # 106.3138 is chosen from this distribution. 
																			  qcauchy(0.95,1/2)
																			  #Hence, there is a 95% probability that a random number less than or equal to
																			  # 6.814 is chosen from this distribution. 


																			  qcauchy(0.20,100)
																			  # We now know that the probability of selecting a number from this 
																			  # distribution that is greater than or equal to 98.62 is 0.80. 

																			  qcauchy(0.20,1/2)
																			  # We now know that the probability of selecting a number from this 
																			  # distribution that is greater than or equal to 0.8763 is 0.80. 
																			  #:::::::::::::::::::::::END 3-CAUCHY DISTRIBUTION:::::::::::::::::::::::::::::::::#

																			  #:::::::::::::::::::::::4-WEIBULL DISTRIBUTION::::::::::::::::::::::::::::::::::::#
																			  # ******** THE PROBABILITY DENSITY FUNCTION ********:

																			  x=seq(0,10,length=200)
																			  y=dweibull(x,3,3)
																			  plot(x,y,type="l",lwd=2,col="blue")

																			  # Q: Check the effect of changing the two parameters and notice the effects on
																			  # this distribution plot.
																			  # What kind  of parameters are these?

																			  # Increasing first parameter:
																			  i=0
																			  for (i in seq(3,15,3))
																			  {
																			    x=seq(0,10,length=200)
																				  y=dweibull(x,i,3)
																				    plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
																					}

																					# Increasing second parameter:
																					i=0
																					for (i in seq(3,10,1.5))
																					{
																					  x=seq(0,10,length=200)
																					    y=dweibull(x,3,i)
																						  plot(x,y,type="l",lwd=2,col="blue",ylim=c(0,3))
																						  }


																						  # A: It is obvious that increasing our first parameter causes a change in shape, while  
																						  # increasing our second parameter causes a shift/scale
																						  # Thus, the first parameter is a shape parameter, while our second
																						  # parameter is a scale parameter

																						  # ******** TESTING QUANTILES ********:
																						  qweibull(0.95,3,3)
																						  #Hence, there is a 95% probability that a random number less than or equal to
																						  # 4.32 is chosen from this distribution. 

																						  qweibull(0.20,3,3)
																						  # We now know that the probability of selecting a number from this 
																						  # distribution that is greater than or equal to 1.820 is 0.80. 

																						  #:::::::::::::::::::::::END 4-WEIBULL DISTRIBUTION:::::::::::::::::::::::::::::::::#

																						  # PART (B) - CHECKING EMPIRICAL RULES ON EACH DISTRIBUTION:

																						  # GAMMA DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rgamma(10000,5,2))
																						  standd = sqrt(var(rgamma(10000,5,2)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dgamma(x,5,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dgamma(x,5,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pgamma(mu + standdScale,5,2) - pgamma(mu - standdScale,5,2) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rgamma(10000,5,2))
																						  standd = sqrt(var(rgamma(10000,5,2)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dgamma(x,5,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dgamma(x,5,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pgamma(mu + standdScale,5,2) - pgamma(mu - standdScale,5,2) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rgamma(10000,5,2))
																						  standd = sqrt(var(rgamma(10000,5,2)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dgamma(x,5,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dgamma(x,5,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pgamma(mu + standdScale,5,2) - pgamma(mu - standdScale,5,2) 

																						  # Looking at the above for this distribution, we see that it almost worked, but
																						  # not just quite. So no.
																						  # 1sd = 70.8%
																						  # 2sd = 95.5%
																						  # 3sd = 99.05%

																						  # EXPONENTIAL DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rexp(10000,2))
																						  standd = sqrt(var(rexp(10000,2)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dexp(x,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dexp(x,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pexp(mu + standdScale,2) - pexp(mu - standdScale,2) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rexp(10000,2))
																						  standd = sqrt(var(rexp(10000,2)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dexp(x,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dexp(x,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pexp(mu + standdScale,2) - pexp(mu - standdScale,2) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rexp(10000,2))
																						  standd = sqrt(var(rexp(10000,2)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dexp(x,2) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dexp(x,2) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pexp(mu + standdScale,2) - pexp(mu - standdScale,2) 

																						  # Looking at the above for this distribution, we see that NO it does not work.
																						  # 1sd = 85%
																						  # 2sd = 95.0%
																						  # 3sd = 97.99%

																						  # CHI SQUARED DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rchisq(10000,df=1))
																						  standd = sqrt(var(rchisq(10000,df=1)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dchisq(x,df=1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dchisq(x,df=1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pchisq(mu + standdScale,1) - pchisq(mu - standdScale,1) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rchisq(10000,df=1))
																						  standd = sqrt(var(rchisq(10000,df=1)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dchisq(x,df=1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dchisq(x,df=1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pchisq(mu + standdScale,1) - pchisq(mu - standdScale,1) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rchisq(10000,df=1))
																						  standd = sqrt(var(rchisq(10000,df=1)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dchisq(x,df=1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dchisq(x,df=1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pchisq(mu + standdScale,1) - pchisq(mu - standdScale,1) 

																						  # Looking at the above for this distribution, we see that NO it does not work.
																						  # 1sd = 88.1%
																						  # 2sd = 94.98%
																						  # 3sd = 97.83%

																						  # BETA DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rbeta(10000,3,3))
																						  standd = sqrt(var(rbeta(10000,3,3)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dbeta(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dbeta(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pbeta(mu + standdScale,3,3) - pbeta(mu - standdScale,3,3) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rbeta(10000,3,3))
																						  standd = sqrt(var(rbeta(10000,3,3)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dbeta(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dbeta(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pbeta(mu + standdScale,3,3) - pbeta(mu - standdScale,3,3) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rbeta(10000,3,3))
																						  standd = sqrt(var(rbeta(10000,3,3)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dbeta(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dbeta(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pbeta(mu + standdScale,3,3) - pbeta(mu - standdScale,3,3) 

																						  # Looking at the above for this distribution, we see that NO it does not work.
																						  # 1sd = 64.6%
																						  # 2sd = 96.7%
																						  # 3sd = 100%

																						  # UNIFORM DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(runif(10000,0,1))
																						  standd = sqrt(var(runif(10000,0,1)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dunif(x,0,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dunif(x,0,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  punif(mu + standdScale,0,1) - punif(mu - standdScale,0,1) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(runif(10000,0,1))
																						  standd = sqrt(var(runif(10000,0,1)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dunif(x,0,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dunif(x,0,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  punif(mu + standdScale,0,1) - punif(mu - standdScale,0,1) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(runif(10000,0,1))
																						  standd = sqrt(var(runif(10000,0,1)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 1, length=200);
																						  y=dunif(x,0,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dunif(x,0,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  punif(mu + standdScale,0,1) - punif(mu - standdScale,0,1) 
																						  # Looking at the above for this distribution, we see that NO it does not work.
																						  # 1sd = 58.4%
																						  # 2sd = 100%
																						  # 3sd = 100%

																						  # CAUCHY DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rcauchy(10000,100,1))
																						  standd = sqrt(var(rcauchy(10000,100,1)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dcauchy(x,100,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dcauchy(x,100,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pcauchy(mu + standdScale,100,1) - pcauchy(mu - standdScale,100,1) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rcauchy(10000,100,1))
																						  standd = sqrt(var(rcauchy(10000,100,1)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dcauchy(x,100,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dcauchy(x,100,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pcauchy(mu + standdScale,100,1) - pcauchy(mu - standdScale,100,1) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rcauchy(10000,100,1))
																						  standd = sqrt(var(rcauchy(10000,100,1)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dcauchy(x,100,1) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dcauchy(x,100,1) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pcauchy(mu + standdScale,100,1) - pcauchy(mu - standdScale,100,1) 

																						  # Looking at the above for this distribution, we see that NO it does not work.
																						  # 1sd = 98.21%
																						  # 2sd = 99.81%
																						  # 3sd = 99.99%

																						  # WEIBULL DISTRIBUTION
																						  # ******** 68%-95%-99.7% Rule {Empirical Rule} ********:
																						  # 1 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rweibull(10000,3,3))
																						  standd = sqrt(var(rweibull(10000,3,3)))
																						  numDeviations = 1
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dweibull(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dweibull(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pweibull(mu + standdScale,3,3) - pweibull(mu - standdScale,3,3) 

																						  # 2 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rweibull(10000,3,3))
																						  standd = sqrt(var(rweibull(10000,3,3)))
																						  numDeviations = 2
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dweibull(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dweibull(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pweibull(mu + standdScale,3,3) - pweibull(mu - standdScale,3,3) 

																						  # 3 SD From mean:
																						  # First find mean, standard deviation, and set scale
																						  mu = mean(rweibull(10000,3,3))
																						  standd = sqrt(var(rweibull(10000,3,3)))
																						  numDeviations = 3
																						  standdScale = numDeviations * standd

																						  x = seq(0, 10, length=200);
																						  y=dweibull(x,3,3) 
																						  plot(x, y, type="l", lwd=2, col="blue");
																						  x=seq(mu - standdScale,mu + standdScale,length=200)
																						  y=dweibull(x,3,3) 
																						  polygon(c(mu - standdScale,x,mu + standdScale),c(0,y,0),col="gray") 

																						  pweibull(mu + standdScale,3,3) - pweibull(mu - standdScale,3,3)  

																						  # Looking at the above for this distribution, we see that althought it is close,
																						  # it does not work.
																						  # 1sd = 66.72%
																						  # 2sd = 96.17%
																						  # 3sd = 99.86%


																						  #PART C: Take a random sample of size 2000 from each distribution.
																						  # Calculate its mean and standard deviation. Does is match the closed 
																						  # form of Expected value and Variance derived in class

																						  # GAMMA
																						  sample = rgamma(2000, 5, 2)
																						  summary(sample)
																						  var(sample)
																						  sqrt(var(sample))

																						  mean=5 * .5; mean
																						  var=5 * .5^2; var
																						  stddev=sqrt(var); stddev

																						  # Yes. We see that the mean and variance closely match the expected value
																						  # and variance equations we derived in class.

																						  # EXPONENTIAL
																						  sample = rexp(2000, 2)
																						  summary(sample)
																						  var(sample)
																						  sqrt(var(sample))

																						  mean=1 / 2; mean
																						  var=1 / (2^2); var
																						  stddev=sqrt(var); stddev

																						  # Yes. We see that the mean and variance closely match the expected value
																						  # and variance equations we derived in class.

																						  # CHI-SQUARED
																						  sample = rchisq(2000, df=1)
																						  summary(sample)
																						  var(sample)
																						  sqrt(var(sample)) #standard dev

																						  mean=1; mean #b/c E[x] = n = 1
																						  var=2; var #b/c Var[x] = 2n = 2
																						  stddev=sqrt(var); stddev

																						  # Yes. We see that the mean and variance closely match the expected value
																						  # and variance equations we derived in class.

																						  # BETA
																						  sample = rbeta(2000, 3,3)
																						  summary(sample)
																						  var(sample)
																						  sqrt(var(sample)) #standard dev

																						  mean=3/(3+3); mean #b/c E[x] = a/(a+b)
																						  var=(3*3)/((3+3)^2*(3+3+1)); var #b/c Var[x] = ab/((a+b)^2 (a+b+1))
																						  stddev=sqrt(var); stddev

																						  # Yes. We see that the mean and variance closely match the expected value
																						  # and variance equations we derived in class.

																						  # UNIFORM
																						  sample = runif(2000, 0,1)
																						  summary(sample)
																						  var(sample)
																						  sqrt(var(sample)) #standard dev

																						  mean=(1+0)/2; mean #b/c E[x] = (a+b)/2
																						  var=(1-0)^2/12; var #b/c Var[x] = (b-a)^2/12
																						  stddev=sqrt(var); stddev

																						  # Yes. We see that the mean and variance closely match the expected value
																						  # and variance equations we derived in class.

																						  # # CAUCHY
																						  # sample = rcauchy(2000, 100,1)
																						  # summary(sample)
																						  # var(sample)
																						  # sqrt(var(sample)) #standard dev
																						  # 
																						  # mean=
																						  # var=
																						  # stddev=sqrt(var); stddev
																						  # 
																						  # sample = rcauchy(2000, 1/2,1)
																						  # summary(sample)
																						  # var(sample)
																						  # sqrt(var(sample)) #standard dev
																						  # 
																						  # mean=
																						  # var=
																						  # stddev=sqrt(var); stddev
																						  # 
																						  # 
																						  # # ??? We see that the mean and variance closely match the expected value
																						  # # and variance equations we derived in class (IN BOTH CASES).
																						  # 
																						  # # WEIBULL
																						  # sample = rweibull(2000, 3,3)
																						  # summary(sample)
																						  # var(sample)
																						  # sqrt(var(sample)) #standard dev
																						  # 
																						  # mean=gamma(1/3 + 1); mean #b/c E[x] = Gamma(1/alpha + 1)
																						  # var=gamma(2/3 + 1) - (gamma(1/3 + 1))^2; var #b/c Var[x] = Gamma(2/alpha + 1) - Gamma^2(1/alpha +1)
																						  # stddev=sqrt(var); stddev
																						  # 
																						  # # Yes. We see that the mean and variance closely match the expected value
																						  # # and variance equations we derived in class.




