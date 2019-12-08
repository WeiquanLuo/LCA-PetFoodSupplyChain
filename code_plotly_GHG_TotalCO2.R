library(plotly)
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- dat$Sector %>% unique() %>% length()
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# calculate with formula only
calculate_formula <- function(data, formula = y~1000*x){
  as.function <- function(formula) {
    cmd <- tail(as.character(formula),1)
    exp <- parse(text=cmd)
    function(...) eval(exp, list(...))
  }
  formula.function <- as.function(formula)
  result<- formula.function(x=data)
  return(result)
}

X <- resource %>% 
  select(Coal.MJ, Petrol.MJ, NatGase.MJ) %>% 
  mutate_all(calculate_formula, formula = y~x+1)
y <- GHG %>% select(Total.g.CO2e)
data <- cbind(ID, y, X)

p <- plot_ly(data, 
             x = ~Coal.MJ, 
             y = ~Petrol.MJ, 
             z = ~NatGase.MJ,
             color = ~Sector, 
             colors = mycolors,
             marker = list(symbol = 'circle',
                           sizemode = 'diameter'),
             size = ~Total.g.CO2e,
             sizes = c(5,150),
             text = ~paste('Sector:', Sector, 
                           '<br>Description:', Description, 
                           '<br>Sector_sub:', Sector_sub, 
                           '<br>name_sub:', name_sub,
                           '<br>Total.g.CO2e:', Total.g.CO2e)) %>%
  layout(title = 'Total CO2 equivalent vs Energy source (Coal, Petrol, NatGase) <br> by NAICS 2002 Sectors',
         scene = list(xaxis = list(title = 'Coal.MJ',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   type = 'log'),
                      yaxis = list(title = 'Petrol.MJ',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   type = 'log'),
                      zaxis = list(title = 'NatGase.MJ',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   type = 'log')),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)',
         annotations = list(x = 1.07,
                            y = 1.015,
                            text = 'Sector by NAICS 2002',
                            showarrow = FALSE))
p

