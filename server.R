library(shiny)
shinyServer(
  function(input, output){
    
    kCall <- function(S){  

      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5

      return(exp(-div*dt)* pnorm(d2))
    }
    
    kPut<-function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
 
      return(exp(-r*dt)*pnorm(-d2))
    }
    
    #call delta
    DeltaCall <- function(S){
      
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return(exp(-div * dt) * pnorm(d1))
    }
    
    #put delta
    DeltaPut <- function(S){
      
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return(-exp(-div * dt) * pnorm(-d1))
    }
    
    
    PriceCall <- function(S){
      K   <- input$strike
      return(S * DeltaCall(S) - K * kCall(S))
    }
    
    
    PricePut  <- function(S){
      K   <- input$strike
      return(K * kPut(S) + S * DeltaPut(S))   #careful with the signs !
    }
    
    
    Gamma <- function(S){
      
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      
      return((1 / (S * v * dt^0.5)) * exp(-div * dt)* dnorm(d1))
    }
    
    
    Vega <- function(S){
      
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      
      return((S * dt^0.5) * exp(-div * dt) * dnorm(d1) / 100)
    }
    
    
    ThetaCall <- function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((-K * exp(-r*dt) * (r*pnorm(d2) + v*dnorm(d2) / (2*dt^0.5))) / 365)  #365 days in the year
    }
    
    
    ThetaPut <- function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((K * exp(-r*dt) * (r*pnorm(-d2) - v*dnorm(d2) / (2*dt^0.5))) / 365)
    }
    
    
    
    RhoCall<-function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((dt * K * exp(-r * dt) * pnorm(d2)) / 100)
    }
    
    
    RhoPut<-function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((-dt * K * exp(-r * dt) * pnorm(-d2)) / 100)
    }
    
    
    PsiCall<-function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((-S * dt * exp(-div * dt) * pnorm(d1)) / 100)
    }
    
    
    PsiPut<-function(S){
      K   <- input$strike
      v   <- input$volatility/100
      r   <- input$r / 100
      div <- input$dividend / 100 
      dt  <- input$time/365 
      d1  <- (log(S / K) + (r - div + 0.5 * v^2) * dt) / (v * dt^0.5)
      d2  <- d1 - v * dt^0.5
      
      return((S * dt * exp(-div * dt) * pnorm(-d1)) / 100)
    }
    
    output$outputs <- renderTable({
      
      S <- input$assetPrice
      K <- input$strike
      
      Outputs <- c("Stock", "Option price", "Delta", "Gamma", "Vega", "Theta", "Rho", "Psi")
      Call <- c( S, PriceCall(S), DeltaCall(S), Gamma(S), Vega(S), ThetaCall(S), RhoCall(S), PsiCall(S))
      Put <-  c( S, PricePut(S), DeltaPut(S), Gamma(S), Vega(S), ThetaPut(S), RhoPut(S), PsiPut(S))
      
      stockcall <- paste(K, " Call") 
      stockput  <- paste(K, " Put") 
      
      greektable <- setNames(data.frame(Outputs, Call, Put), c("Outputs", stockcall, stockput))
      
      print(format(greektable, digits=2))
    })
    
    
  }
)


