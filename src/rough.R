library(data.table)

# Calculate proportion responsible for 80% of total transmissions
propresponsible=function(R0,k,prop){
 qm1=qnbinom(1-prop,k+1,mu=R0*(k+1)/k)
 remq=1-prop-pnbinom(qm1-1,k+1,mu=R0*(k+1)/k)
 remx=remq/dnbinom(qm1,k+1,mu=R0*(k+1)/k)
 q=qm1+1
 1-pnbinom(q-1,k,mu=R0)-dnbinom(q,k,mu=R0)*remx
}

propresponsible(R0 = 0.429, k = 0.265, prop = 0.8)

# simulate runs from R0 and k distributions

# name    est     sd est_low est_upper
# k     0.265 0.0339   0.198     0.331
# R0    0.429 0.0349   0.361     0.497

#This allows us to simulate the number of infected by applying the distribution defined above
different_fit_run <- 
 data.frame(rbindlist(sapply(1:100, function(x){
  
  simulated_infected <- rnbinom(n = ebola_data$contacts_infected, size = nb_fit$estimate[1], mu = nb_fit$estimate[2])
  fit_dist_simulated <- fitdist(simulated_infected, "nbinom")
  data.frame(type = c("R0", "k", "Prop responsible"),
             value = c(fit_dist_simulated$estimate[2], fit_dist_simulated$estimate[1],
                       propresponsible(fit_dist_simulated$estimate[2], fit_dist_simulated$estimate[1], .8)))
 }, simplify = FALSE))
 )
