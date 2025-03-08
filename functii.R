#Problema 2
#subpunctul 1
library(shiny)  # Importăm pachetul Shiny pentru crearea aplicației web 

library(ggplot2)  # Importăm ggplot2 pentru generarea graficelor 



# Definim UI (User Interface) - interfața utilizatorului 

ui <- fluidPage( 
  
  titlePanel("Functii de repartitie"),  # Titlul aplicației 
  
  
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      selectInput("var_choice", "Alege variabila:",  # Meniu dropdown pentru alegerea variabilei 
                  
                  choices = c("X", "3 + 2X", "X^2", "Suma X", "Suma X^2")), 
      
      sliderInput("n", "Numar de variabile (n):", min = 1, max = 10000, value = 10)  # Slider pentru alegerea lui n 
      
    ), 
    
    
    
    mainPanel( 
      
      plotOutput("cdfPlot")  # Afișăm graficul CDF (funcția de repartiție cumulativă) 
      
    ) 
    
  ) 
  
) 



# Definim partea de server a aplicației 

server <- function(input, output) { 
  
  
  
  output$cdfPlot <- renderPlot({ 
    
    n <- input$n  # Obținem valoarea lui n de la slider 
    
    X <- rnorm(10000)  # Generăm 10.000 de valori aleatoare din distribuția normală N(0,1) 
    
    
    
    # Alegem transformarea corespunzătoare în funcție de selecția utilizatorului 
    
    values <- switch(input$var_choice, 
                     
                     "X" = X,  # Variabila inițială X ~ N(0,1) 
                     
                     "3 + 2X" = 3 + 2 * X,  # Transformare liniară a lui X 
                     
                     "X^2" = X^2,  # X ridicat la pătrat 
                     
                     "Suma X" = rowSums(matrix(rnorm(10000 * n), ncol = n)),  # Sumă de n variabile normale 
                     
                     "Suma X^2" = rowSums(matrix(rnorm(10000 * n)^2, ncol = n)))  # Sumă de pătrate ale variabilelor normale 
    
    
    
    df <- data.frame(values)  # Creăm un dataframe pentru utilizare în ggplot 
    
    ggplot(df, aes(x = values)) +  # Inițiem graficul cu variabila values 
      
      stat_ecdf(geom = "step", color = "blue") +  # Afișăm funcția de repartiție empirică (ECDF) ca o curbă în trepte 
      
      labs(title = paste("Functia de repartitie pentru", input$var_choice),  # Adăugăm titlul graficului 
           
           x = "Valoare", y = "F(x)") +  # Setăm etichetele axelor 
      
      theme_minimal()  # Aplicăm un stil minimalist graficului 
    
  }) 
  
} 



shinyApp(ui = ui, server = server) 







#subpunctul 2

library(shiny)  # Importăm pachetul Shiny pentru crearea aplicației web 

library(ggplot2)  # Importăm ggplot2 pentru generarea graficelor 



# Definim UI (User Interface) - interfața utilizatorului 

ui <- fluidPage( 
  
  titlePanel("Functii de repartitie"),  # Titlul aplicației 
  
  
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      selectInput("var_choice", "Alege variabila:",  # Meniu dropdown pentru alegerea variabilei 
                  
                  choices = c("X", "3 + 2X", "X^2", "Suma X", "Suma X^2")), 
      
      sliderInput("n", "Numar de variabile (n):", min = 1, max = 10000, value = 10),  # Slider pentru alegerea lui n 
      
      numericInput("mu", "Media (µ):", value = 0),  # Input pentru µ 
      
      numericInput("sigma", "Deviatia standard (σ):", value = 1, min = 0.01)  # Input pentru σ, pozitiv 
      
    ), 
    
    
    
    mainPanel( 
      
      plotOutput("cdfPlot")  # Afișăm graficul CDF (funcția de repartiție cumulativă) 
      
    ) 
    
  ) 
  
) 



# Definim partea de server a aplicației 

server <- function(input, output) { 
  
  
  
  output$cdfPlot <- renderPlot({ 
    
    n <- input$n  # Obținem valoarea lui n de la slider 
    
    mu <- input$mu  # Obținem µ de la utilizator 
    
    sigma <- input$sigma  # Obținem σ de la utilizator 
    
    X <- rnorm(10000, mean = mu, sd = sigma)  # Generăm 10.000 de valori din N(µ, σ^2) 
    
    
    
    # Alegem transformarea corespunzătoare în funcție de selecția utilizatorului 
    
    values <- switch(input$var_choice, 
                     
                     "X" = X,  # Variabila inițială X ~ N(µ,σ^2) 
                     
                     "3 + 2X" = 3 + 2 * X,  # Transformare liniară a lui X 
                     
                     "X^2" = X^2,  # X ridicat la pătrat 
                     
                     "Suma X" = rowSums(matrix(rnorm(10000 * n, mean = mu, sd = sigma), ncol = n)),  # Sumă de n variabile normale 
                     
                     "Suma X^2" = rowSums(matrix(rnorm(10000 * n, mean = mu, sd = sigma)^2, ncol = n)))  # Sumă de pătrate ale variabilelor normale 
    
    
    
    df <- data.frame(values)  # Creăm un dataframe pentru utilizare în ggplot 
    
    ggplot(df, aes(x = values)) +  # Inițiem graficul cu variabila values 
      
      stat_ecdf(geom = "step", color = "blue") +  # Afișăm funcția de repartiție empirică (ECDF) ca o curbă în trepte 
      
      labs(title = paste("Functia de repartitie pentru", input$var_choice),  # Adăugăm titlul graficului 
           
           x = "Valoare", y = "F(x)") +  # Setăm etichetele axelor 
      
      theme_minimal()  # Aplicăm un stil minimalist graficului 
    
  }) 
  
} 



shinyApp(ui = ui, server = server) 









#subpunctul 3
library(shiny)  # Importăm pachetul Shiny pentru crearea aplicației web 

library(ggplot2)  # Importăm ggplot2 pentru generarea graficelor 



# Definim UI (User Interface) - interfața utilizatorului 

ui <- fluidPage( 
  
  titlePanel("Functii de repartitie"),  # Titlul aplicației 
  
  
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      selectInput("var_choice", "Alege variabila:",  # Meniu dropdown pentru alegerea variabilei 
                  
                  choices = c("X", "2 - 5X", "X^2", "Suma X")), 
      
      sliderInput("n", "Numar de variabile (n):", min = 1, max = 10000, value = 10),  # Slider pentru alegerea lui n 
      
      numericInput("lambda", "Parametrul lambda (λ):", value = 1, min = 0.01)  # Input pentru λ, pozitiv 
      
    ), 
    
    
    
    mainPanel( 
      
      plotOutput("cdfPlot")  # Afișăm graficul CDF (funcția de repartiție cumulativă) 
      
    ) 
    
  ) 
  
) 



# Definim partea de server a aplicației 

server <- function(input, output) { 
  
  
  
  output$cdfPlot <- renderPlot({ 
    
    n <- input$n  # Obținem valoarea lui n de la slider 
    
    lambda <- input$lambda  # Obținem λ de la utilizator 
    
    X <- rexp(10000, rate = lambda)  # Generăm 10.000 de valori din Exp(λ) 
    
    
    
    # Alegem transformarea corespunzătoare în funcție de selecția utilizatorului 
    
    values <- switch(input$var_choice, 
                     
                     "X" = X,  # Variabila inițială X ~ Exp(λ) 
                     
                     "2 - 5X" = 2 - 5 * X,  # Transformare liniară a lui X 
                     
                     "X^2" = X^2,  # X ridicat la pătrat 
                     
                     "Suma X" = rowSums(matrix(rexp(10000 * n, rate = lambda), ncol = n)))  # Sumă de n variabile exponențiale 
    
    
    
    df <- data.frame(values)  # Creăm un dataframe pentru utilizare în ggplot 
    
    ggplot(df, aes(x = values)) +  # Inițiem graficul cu variabila values 
      
      stat_ecdf(geom = "step", color = "blue") +  # Afișăm funcția de repartiție empirică (ECDF) ca o curbă în trepte 
      
      labs(title = paste("Functia de repartitie pentru", input$var_choice),  # Adăugăm titlul graficului 
           
           x = "Valoare", y = "F(x)") +  # Setăm etichetele axelor 
      
      theme_minimal()  # Aplicăm un stil minimalist graficului 
    
  }) 
  
} 



shinyApp(ui = ui, server = server) 









#subpunctul 4
library(shiny)  # Importăm pachetul Shiny pentru crearea aplicației web 

library(ggplot2)  # Importăm ggplot2 pentru generarea graficelor 



# Definim UI (User Interface) - interfața utilizatorului 

ui <- fluidPage( 
  
  titlePanel("Functii de repartitie"),  # Titlul aplicației 
  
  
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      selectInput("var_choice", "Alege variabila:",  # Meniu dropdown pentru alegerea variabilei 
                  
                  choices = c("X", "3X + 2", "X^2", "Suma X")), 
      
      sliderInput("n", "Numar de variabile (n):", min = 1, max = 10000, value = 10),  # Slider pentru alegerea lui n 
      
      numericInput("lambda", "Parametrul lambda (λ):", value = 1, min = 0.01)  # Input pentru λ, pozitiv 
      
    ), 
    
    
    
    mainPanel( 
      
      plotOutput("cdfPlot")  # Afișăm graficul CDF (funcția de repartiție cumulativă) 
      
    ) 
    
  ) 
  
) 



# Definim partea de server a aplicației 

server <- function(input, output) { 
  
  
  
  output$cdfPlot <- renderPlot({ 
    
    n <- input$n  # Obținem valoarea lui n de la slider 
    
    lambda <- input$lambda  # Obținem λ de la utilizator 
    
    X <- rpois(10000, lambda)  # Generăm 10.000 de valori din Pois(λ) 
    
    
    
    # Alegem transformarea corespunzătoare în funcție de selecția utilizatorului 
    
    values <- switch(input$var_choice, 
                     
                     "X" = X,  # Variabila inițială X ~ Pois(λ) 
                     
                     "3X + 2" = 3 * X + 2,  # Transformare liniară a lui X 
                     
                     "X^2" = X^2,  # X ridicat la pătrat 
                     
                     "Suma X" = rowSums(matrix(rpois(10000 * n, lambda), ncol = n)))  # Sumă de n variabile Poisson 
    
    
    
    df <- data.frame(values)  # Creăm un dataframe pentru utilizare în ggplot 
    
    ggplot(df, aes(x = values)) +  # Inițiem graficul cu variabila values 
      
      stat_ecdf(geom = "step", color = "blue") +  # Afișăm funcția de repartiție empirică (ECDF) ca o curbă în trepte 
      
      labs(title = paste("Functia de repartitie pentru", input$var_choice),  # Adăugăm titlul graficului 
           
           x = "Valoare", y = "F(x)") +  # Setăm etichetele axelor 
      
      theme_minimal()  # Aplicăm un stil minimalist graficului 
    
  }) 
  
} 



shinyApp(ui = ui, server = server) 












#subpunctul 5
library(shiny)  # Importăm pachetul Shiny pentru crearea aplicației web 

library(ggplot2)  # Importăm ggplot2 pentru generarea graficelor 



# Definim UI (User Interface) - interfața utilizatorului 

ui <- fluidPage( 
  
  titlePanel("Functii de repartitie - Binomiala"),  # Titlul aplicației 
  
  
  
  sidebarLayout( 
    
    sidebarPanel( 
      
      selectInput("var_choice", "Alege variabila:",  # Meniu dropdown pentru alegerea variabilei 
                  
                  choices = c("X", "5X + 4", "X^3", "Suma X")),  # Opțiuni pentru variabilele de selectat 
      
      sliderInput("n", "Numar de variabile (n):", min = 1, max = 10000, value = 10),  # Slider pentru alegerea lui n 
      
      numericInput("r", "Parametrul r:", value = 10, min = 1),  # Input pentru parametrul r (numărul de încercări) 
      
      numericInput("p", "Parametrul p (0 < p < 1):", value = 0.5, min = 0.01, max = 0.99)  # Input pentru parametrul p (probabilitatea de succes) 
      
    ), 
    
    
    
    mainPanel( 
      
      plotOutput("cdfPlot")  # Afișăm graficul CDF (funcția de repartiție cumulativă) 
      
    ) 
    
  ) 
  
) 



# Definim partea de server a aplicației 

server <- function(input, output) { 
  
  
  
  output$cdfPlot <- renderPlot({ 
    
    n <- input$n  # Obținem valoarea lui n de la slider 
    
    r <- input$r  # Obținem r de la utilizator 
    
    p <- input$p  # Obținem p de la utilizator 
    
    X <- rbinom(10000, r, p)  # Generăm 10.000 de valori din Binomial(r, p) 
    
    
    
    # Alegem transformarea corespunzătoare în funcție de selecția utilizatorului 
    
    values <- switch(input$var_choice, 
                     
                     "X" = X,  # Variabila inițială X ~ Binom(r, p) 
                     
                     "5X + 4" = 5 * X + 4,  # Transformare liniară a lui X 
                     
                     "X^3" = X^3,  # X ridicat la cub 
                     
                     "Suma X" = rowSums(matrix(rbinom(10000 * n, r, p), ncol = n)))  # Sumă de n variabile Binomial 
    
    
    
    df <- data.frame(values)  # Creăm un dataframe pentru utilizare în ggplot 
    
    ggplot(df, aes(x = values)) +  # Inițiem graficul cu variabila values 
      
      stat_ecdf(geom = "step", color = "blue") +  # Afișăm funcția de repartiție empirică (ECDF) ca o curbă în trepte 
      
      labs(title = paste("Functia de repartitie pentru", input$var_choice),  # Adăugăm titlul graficului 
           
           x = "Valoare", y = "F(x)") +  # Setăm etichetele axelor 
      
      theme_minimal()  # Aplicăm un stil minimalist graficului 
    
  }) 
  
} 



shinyApp(ui = ui, server = server) 