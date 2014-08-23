library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Black Scholes Calculator"),
  sidebarPanel(
    h4('Input parameters'),
    numericInput('assetPrice', 'Spot price of underlying asset ($)', 100),
    numericInput('strike', 'Strike Price of the option ($)', 110),
    numericInput('r', 'Risk free interest rate (%)', 5) ,
    numericInput('volatility', 'Volatility (%)', 20),
    numericInput('time', 'Time to maturity (days)', 45),
    numericInput('dividend', 'Dividend (%)', 7)
  ),
  mainPanel(
    h3('Summary'),
    helpText("This application uses Black-Scholes pricing model to calculate the is for equity investors to calculate the theoretical price and greeks based on the input parameters."),
    helpText("Modify the input parameters "),

    mainPanel(
      h4('Option prices and greeks'),       
      tableOutput("outputs")
      


      
    ),

    br()
  )
))