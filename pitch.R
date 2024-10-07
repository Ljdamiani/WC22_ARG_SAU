create_StatsBomb_Pitch_Vertical <- function(grass_colour, line_colour, background_colour, goal_colour, BasicFeatures){
  
  theme_blankPitch = function(size=12) { 
    theme(
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      axis.ticks.length=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
  # Coordenadas ajustadas para o campo vertical
  xmin <- 0 # minimum width
  xmax <- 80 # maximum width
  ymin <- 0 # minimum length
  ymax <- 120 # maximum length
  
  # Definindo elementos ao longo do comprimento
  boxEdgeDef <- 18
  boxEdgeOff <- 102
  halfwayline <- 60
  sixYardDef <- 6
  sixYardOff <- 114
  penSpotDef <- 12
  penSpotOff <- 108
  
  # Definindo elementos ao longo da largura
  boxEdgeLeft <- 18
  boxEdgeRight <- 62
  sixYardLeft <- 30 
  sixYardRight <- 50
  goalPostLeft <- 36
  goalPostRight <- 44
  CentreSpot <- 40   
  
  # Outras dimensões
  centreCircle_d <- 20     
  
  ## Função para desenhar o círculo central
  circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0, 2*pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  #### Criando o círculo central ####
  center_circle <- circleFun(c(CentreSpot, halfwayline), centreCircle_d, npoints = 100)
  
  ## Iniciando o plot vertical
  p <- ggplot() + xlim(c(xmin-5, xmax+5)) + ylim(c(ymin-5, ymax+5)) +
    theme_blankPitch() +
    # Adicionando o campo base
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill = grass_colour, colour = line_colour) +
    # Adicionando a área de 18 jardas defensiva
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=ymin, ymax=boxEdgeDef), fill = grass_colour, colour = line_colour) +
    # Adicionando a área de 18 jardas ofensiva
    geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=ymax), fill = grass_colour, colour = line_colour) +
    # Adicionando a linha do meio-campo
    geom_segment(aes(x = xmin, y = halfwayline, xend = xmax, yend = halfwayline), colour = line_colour) +
    # Adicionando a pequena área defensiva
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=ymin, ymax=sixYardDef), fill = grass_colour, colour = line_colour) +
    # Adicionando a pequena área ofensiva
    geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=ymax), fill = grass_colour, colour = line_colour) +
    # Adicionando o círculo central
    geom_path(data=center_circle, aes(x=x, y=y), colour = line_colour) +
    # Adicionando os pontos de pênalti
    geom_point(aes(x = CentreSpot , y = penSpotDef), colour = line_colour, size = 0.5) + 
    geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour, size = 0.5) +
    # Adicionando o ponto central
    geom_point(aes(x = CentreSpot , y = halfwayline), colour = line_colour) +
    # Adicionando o gol defensivo
    geom_segment(aes(x = goalPostLeft, y = ymin, xend = goalPostRight, yend = ymin), colour = goal_colour, size = 1) +
    # Adicionando o gol ofensivo
    geom_segment(aes(x = goalPostLeft, y = ymax, xend = goalPostRight, yend = ymax), colour = goal_colour, size = 1)
  
  return(p)
}

# Exemplo de uso da função para plotar o campo verticalmente
create_StatsBomb_Pitch_Vertical(grass_colour = "green", line_colour = "white", background_colour = "green", goal_colour = "yellow", BasicFeatures = FALSE)
