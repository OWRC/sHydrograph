
library("mgcv")

GAM <- function(df, k=20) {
  # df <- subset(df, !is.na(val))
  # https://www.r-bloggers.com/2016/12/simultaneous-intervals-for-smooths-revisited/
  # see also gam-test.R
  # simultaneous interval for a penalised spline in a fitted GAM
  m <- gam(Val ~ s(doy, k = k), data = df) #, method = "REML")
  #  k = 20 is the basis dimension of the spline
  #  method = “REML”, the penalised spline model is expressed as a linear mixed model with the wiggly bits of the spline treated as random effects, and is estimated using restricted maximum likelihood; method = “ML” would also work here
  
  mVar <- gam(residuals(m)^2 ~ s(doy), data = df)
  df$sd <- sqrt(fitted(mVar))
  df$fit <- fitted(m)
  
  s <- summary(m)
  txt <- paste0("r² = ",round(s$r.sq,4),"  Deviance explained = ", round(s$dev.expl*100,2), "%")
  
  df %>% 
    mutate(daydate = as.Date(doy, origin = "2016-01-01")) %>%
    ggplot(aes(x=daydate)) +
    theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
    theme(axis.title.x = element_blank()) +
    theme(text = element_text(size = 15),
          axis.ticks.length.x = unit(0.5, "cm"),
          axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
    
    geom_ribbon(aes(ymin = fit - qnorm(0.975)*sd, ymax = fit + qnorm(0.975)*sd), alpha = 0.65, fill = "#f1a340") +
    geom_ribbon(aes(ymin = fit - qnorm(.84)*sd, ymax = fit + qnorm(.84)*sd), alpha = 0.65, fill = "#998ec3") +
    
    geom_line(aes(y=fit),size=1, colour='white') +
    
    annotate("text", as.Date("2016-01-01"), -Inf, label = txt, hjust = 0, vjust = -1) +
    
    scale_x_date(date_labels = "%b", date_minor_breaks = "1 month")
}



# GAM <- function(df, k=20, N=10000) {
#   
#   # https://www.r-bloggers.com/2016/12/simultaneous-intervals-for-smooths-revisited/
#   # see also gam-test.R
#   
#   # simultaneous interval for a penalised spline in a fitted GAM
#   m <- gam(val ~ s(doy, k = k), data = df, method = "REML")
#   #  k = 20 is the basis dimension of the spline
#   #  method = “REML”, the penalised spline model is expressed as a linear mixed model with the wiggly bits of the spline treated as random effects, and is estimated using restricted maximum likelihood; method = “ML” would also work here
#   
#   # # The fitted penalised spline with approximate 95% point-wise confidence interval, as produced with plot.gam()
#   # print(summary(m))
#   # plot(m, shade = TRUE, seWithMean = TRUE, residuals = TRUE, pch = 16, cex = 0.8)
#   s <- summary(m)
#   txt <- paste0("r² = ",round(s$r.sq,4),"  Deviance explained = ", round(s$dev.expl*100,2), "%")
#   # print(s$r.sq) # The adjusted r-squared for the model. Defined as the proportion of variance explained, where original variance and residual variance are both estimated using unbiased estimators. This quantity can be negative if your model is worse than a one parameter constant model, and can be higher for the smaller of two nested models! The proportion null deviance explained is probably more appropriate for non-normal errors. Note that r.sq does not include any offset in the one parameter model.
#   # print(s$dev.expl) # The proportion of the null deviance explained by the model. The null deviance is computed taking account of any offset, so dev.expl can be substantially lower than r.sq when an offset is present.
#   
#   # function to generate random values from a multivariate normal:
#   rmvn <- function(n, mu, sig) { ## MVN random deviates
#     L <- mroot(sig)
#     m <- ncol(L)
#     t(mu + L %*% matrix(rnorm(m*n), m, n))
#   }
# 
#   # extract a few things that we need from the fitted GAM
#   Vb <- vcov(m) #  Bayesian covariance matrix of the model coefficients
#   newd <- with(df, data.frame(doy = seq(min(doy), max(doy), length = 200))) # we define our grid of (x) values
#   pred <- predict(m, newd, se.fit = TRUE) # generate predictions and standard errors from the model for the grid of values
#   se.fit <- pred$se.fit # extract the standard errors of the fitted values
# 
# 
#   # we want N draws from which approximately distributed multivariate normals with mean vectors and covariance matrix Vb
#   set.seed(as.numeric(Sys.time()))
#   BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
# 
# 
#   # Now we calculate ((x) - f(x)), evaluated at the grid of (x) values
#   Cg <- predict(m, newd, type = "lpmatrix") # evaluates the basis function
#   simDev <- Cg %*% t(BUdiff) # computes the deviations between the fitted and true parameters
# 
# 
#   # find the absolute values of the standardized deviations from the true model
#   absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
# 
#   # maximum of the absolute standardized deviations at the grid of (x) values for each simulation
#   masd <- apply(absDev, 2L, max)
# 
#   # # ORIGINAL
#   # # scale the standard errors to yield the simultaneous interval; here we calculate the critical value for a 95% simultaneous confidence interval/band
#   # crit <- quantile(masd, prob = 0.95, type = 8) # 95% simultaneous confidence interval for a penalised spline
#   # 
#   # # calculate the simultaneous confidence interval
#   # # Now that we have the critical value, we can calculate the simultaneous confidence interval. In the code block below I first add the grid of values (newd) to the fitted values and standard errors at those new values and then augment this with upper and lower limits for a 95% simultaneous confidence interval (uprS and lwrS), as well as the usual 95% point-wise intervals for comparison (uprP and lwrP).
#   # pred <- transform(cbind(data.frame(pred), newd),
#   #                   uprP = fit + (2 * se.fit),
#   #                   lwrP = fit - (2 * se.fit),
#   #                   uprS = fit + (crit * se.fit),
#   #                   lwrS = fit - (crit * se.fit))
#   # # Comparison of point-wise and simultaneous 95% confidence intervals for the fitted GAM
#   # ggplot(pred, aes(x = doy)) +
#   #   geom_ribbon(aes(ymin = lwrS, ymax = uprS), alpha = 0.2, fill = "red") +
#   #   geom_ribbon(aes(ymin = lwrP, ymax = uprP), alpha = 0.2, fill = "red") +
#   #   labs(y = "Strontium isotope ratio",
#   #        x = "Age [Ma BP]")
# 
#   # scale the standard errors to yield the simultaneous interval; here we calculate the critical value for a 95% simultaneous confidence interval/band
#   scale95 <- quantile(masd, prob = 0.95, type = 8) # 95% simultaneous confidence interval for a penalised spline
#   scale50 <- quantile(masd, prob = 0.5, type = 8) # 50% simultaneous confidence interval for a penalised spline
#   
#   # calculate the simultaneous confidence interval
#   # Now that we have the critical value, we can calculate the simultaneous confidence interval. In the code block below I first add the grid of values (newd) to the fitted values and standard errors at those new values and then augment this with upper and lower limits for a 95% simultaneous confidence interval (uprS and lwrS), as well as the usual 95% point-wise intervals for comparison (uprP and lwrP).
#   transform(cbind(data.frame(pred), newd),
#             upr2 = fit + (scale95 * se.fit),
#             lwr2 = fit - (scale95 * se.fit),
#             upr1 = fit + (scale50 * se.fit),
#             lwr1 = fit - (scale50 * se.fit)) %>%
#     
#     mutate(daydate = as.Date(doy, origin = "2016-01-01")) %>%
#   
#     ggplot(aes(x=daydate)) +
#       theme_bw() + #theme(panel.grid.major = element_line(colour = "#808080"), panel.grid.minor = element_line(colour = "#808080")) +
#       theme(axis.title.x = element_blank()) +
#       theme(text = element_text(size = 15),
#             axis.ticks.length.x = unit(0.5, "cm"),
#             axis.text.x = element_text(vjust = 5.5, hjust = -0.2)) +
#       
#       # # geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.2, fill = "red") +
#       # # geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.2, fill = "blue") +
#       geom_ribbon(aes(ymin = lwr2, ymax = upr2), alpha = 0.75, fill = "#f1a340") +
#       geom_ribbon(aes(ymin = lwr1, ymax = upr1), alpha = 0.75, fill = "#998ec3") +
#     
#       annotate("text", as.Date("2016-01-01"), -Inf, label = txt, hjust = 0, vjust = -1) +
#       
#       scale_x_date(date_labels = "%b", date_minor_breaks = "1 month")
# }
# 


# dfgam <- fossil %>%
#   mutate(doy=age, val=strontium.ratio) %>%
#   select(doy,val)
# 
# GAM(dfgam)
