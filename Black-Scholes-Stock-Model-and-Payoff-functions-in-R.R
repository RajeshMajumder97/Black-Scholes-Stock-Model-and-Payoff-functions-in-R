Black_Scholes_function =function(S,V,r,t,K,simulations){
  
  # S           = underlying price
  # V           = Black_Scholes_function
  # r           = Black_Scholes_function
  # t           = The time to expiration
  # K           = Strike Price
  # simulations = No. of simulations
  payoffs = NULL
  asset_price = NULL
  
  
  generate_asset_price = function(S,V,r,t){
    
    return(S*exp((r-0.5*V^2)*t+V*sqrt(t)*rnorm(1,0,1)))
  }
  
  
  call_payoff= function(S_T,K){
    
    return(max(0,S_T-K))
  }
  
  
  discount_factor = exp(-r *t)
  
  
  
  for(i in seq(1,simulations,1))
  {
    S_T = generate_asset_price(S,V,r,t)
    payoffs[i] =call_payoff(S_T,K)
    asset_price[i] = S_T
  }
  
  
  price = discount_factor*(sum(payoffs)/simulations)
  
  Ans =list(Estimate = paste("The","expected","Price",":",price,sep = " "),
            Plot_1 = plot(asset_price,
                          type="l",
                          ylab=expression(S(t)),
                          xlab=expression(t),
                          main="Plot of S(T)"),
            Plot_2 = plot(asset_price,payoffs,
                          type = "l",
                          xlab = expression(S),
                          ylab = expression(f(S)),
                          main = "S(T) v/s payoff"))
  return(Ans)
  
}


##--- An Example ---#
Solution = Black_Scholes_function(S=857.29,
                                  V=0.2076,
                                  r=0.0014,
                                  t=5/365,
                                  K=860,
                                  simulations = 90000)

##--- Estimated value ---##

Solution[[1]]