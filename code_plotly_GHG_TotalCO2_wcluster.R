library(plotly)
library(RColorBrewer)

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

# const data
X <- resource %>% 
  select(Coal.MJ, Petrol.MJ, NatGase.MJ) %>% 
  mutate_all(calculate_formula, formula = y~x+1)
y <- GHG %>% select(Total.g.CO2e)
data <- cbind(ID, y, X)

# mask log(y)=0
Lny <- log(y) %>% flatten() %>% unlist
data <- data[!is.infinite(Lny),]

# Define the number of colors you want
mysymbols <- c("diamond", "circle" , "square")
labels <- py$cluster_labels %>% as.factor()
nb.cols <- dat$Sector %>% unique() %>% length()
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# plotly
p <- plot_ly(data, 
             x = ~Coal.MJ, 
             y = ~Petrol.MJ, 
             z = ~NatGase.MJ,
             hovertemplate = paste('<b>%{text}</b>',
                                   '<br>Coal.MJ: %{x}',
                                   '<br>Petrol.MJ: %{y}',
                                   '<br>Gase.MJ: %{z}'),
             symbol = ~labels,
             symbols = mysymbols,
             marker = list(sizemode = 'diameter'),
             size = ~Total.g.CO2e,
             sizes = c(5,150),
             text = ~paste('<br>Sector:', Sector, 
                           '<br>Description:', Description, 
                           '<br>Sector_sub:', Sector_sub, 
                           '<br>name_sub:', name_sub,
                           '<br>Total.g.CO2e:', Total.g.CO2e,
                           '<br>cluster:', labels)) %>%
  layout(title = 'Total CO2 equivalent vs Energy source (Coal, Petrol, NatGase) <br> by clusters',
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

htmlwidgets::saveWidget(ggplotly(p),
                        file ="plotly_GHG_TotalCO2_wcluster.html")
