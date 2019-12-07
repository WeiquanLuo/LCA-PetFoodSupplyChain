library(plotly)
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- dat$Sector %>% unique() %>% length()
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

p <- plot_ly(dat, 
             x = ~NonFossElec.MJ, 
             y = ~Petrol.MJ, 
             z = ~NatGase.MJ,
             color = ~Sector, 
             colors = mycolors,
             marker = list(symbol = 'circle',
                           sizemode = 'diameter',
                           size = ~ Total.g.CO2e*10^-6),
             text = ~paste('Sector:', Sector, 
                           '<br>Description:', Description, 
                           '<br>name_sub:', name_sub,
                           '<br>Sector_sub:', Sector_sub, 
                           '<br>Total.t.CO2e', Total.g.CO2e)) %>%
  layout(title = 'Total CO2 equivalent vs Energy source (NonFossElec, Petrol, NatGase) <br> by NAICS 2002 Sectors',
         scene = list(xaxis = list(title = 'NonFossElec.MJ',
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
                            showarrow = FALSE
         ))
p

api_create(p, filename = "few_GHG_example")

