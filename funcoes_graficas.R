library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)

pct_format = scales::percent_format(accuracy = .1)



# Gráfico de barras univariado --------------------------------------------

uni_bar <- function(banco, var, ylab= 'Frequência', xlab = '', color = '#767F8B', label_size = 0.25, order = NULL, flip = F, capitalize=T){
  
  banco <- banco[banco[var] != '',][var]
  
  if(typeof(banco[3,var]) == 'character')
  {  
    prim.maiuscula <- function(x) {
      x <- tolower(x)
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    if(capitalize == T){
    banco[var] <- sapply(banco[var], FUN = prim.maiuscula)}
  }
  
  
  if(!is.null(order))
  {banco[var] <- fct_relevel(factor((banco[[var]]), levels=order))}
  banco <- data.frame(table(banco[var]))
  names(banco)[1] <- var
  
  
  if(flip == F)
  { ax <- ggplot(banco, aes(x = .data[[var]], y = Freq)) + 
    geom_bar(fill=color, stat='identity') + 
    theme_minimal()+
    labs(x= xlab, y = 'Frequência') + 
    ylim(0.0, max(banco$Freq)*1.05)+
    geom_label(aes(
      label = sprintf(
        '%d (%s)',
        Freq,
        pct_format(Freq / sum(Freq))
      )), stat='identity', fill='white', vjust=1.2, label.size = label_size)}
  else if(flip == T)
  { ax <- ggplot(banco, aes(x = .data[[var]], y = Freq)) + 
    geom_bar(fill=color, stat='identity') + 
    theme_minimal() +
    labs(x= xlab, y = 'Frequência') +
    ylim(0.0, max(banco$Freq)*1.05)+
    geom_label(aes(
      label = sprintf(
        '%d (%s)',
        Freq,
        pct_format(Freq / sum(Freq))
      )), stat='identity', fill='white', label.size = label_size)+
    coord_flip()}
  
  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)
  return(ax)
}

# Gráfico de barras bivariado ---------------------------------------------

bi_bar <- function(banco, x, y, ylab= 'Frequência', xlab = '', label_fill = '', 
                   color =  c('#767F8B', '#B3B7B8', '#9FB6CD', '#E5E5E5','#6E7B8B', '#7C878E'),
                   order = NULL, flip = F, capitalize = T){
  
  banco <- banco[banco[x] != '' & banco[y] != '',][,c(x,y)]
  prim.maiuscula <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  if(capitalize == T){
    if(typeof(banco[3,x]) == 'character') {banco[x] <- sapply(banco[x], FUN = prim.maiuscula)}
    if(typeof(banco[3,y]) == 'character') {banco[y] <- sapply(banco[y], FUN = prim.maiuscula)}
  }
  
  if(!is.null(order))
  {banco[x] <- fct_relevel(factor((banco[[x]]), levels=order))}
  banco <- data.frame(table(banco))
  names(banco) <- c(x, y, 'Freq')
  
  
  if(flip == F)
  { 
    ax <- ggplot(banco, aes(x = .data[[x]], y = Freq, fill = .data[[y]])) + 
      geom_bar( stat='identity', position =  "dodge") + 
      theme_minimal()+
      labs(x= xlab, y = 'Frequência', fill = label_fill) + 
      ylim(0.0, max(banco$Freq)*1.05)+
      geom_label(aes(
        label = sprintf(
          '%d (%s)',
          Freq,
          pct_format((Freq / sum(Freq)))
        ), group = y), position = position_dodge2(width = 0.9),
        size=2.5,col = "black", fill='white')+
      scale_fill_manual(values=color)+
      theme_minimal()
    
  }
  
  else if(flip == T)
  {     ax <- ggplot(banco, aes(x = .data[[x]], y = Freq, fill = .data[[y]])) + 
    geom_bar( stat='identity', position =  "dodge") + 
    theme_minimal()+
    labs(x= xlab, y = 'Frequência', fill = label_fill) + 
    ylim(0.0, max(banco$Freq)*1.05)+
    geom_label(aes(
      label = sprintf(
        '%d (%s)',
        Freq,
        pct_format((Freq / sum(Freq)))
      ), group = y), position = position_dodge2(width = 0.9 ),
      size=2.5,hjust= 0.5, col = "black", fill='white')+
    scale_fill_manual(values=color,)+
    theme_minimal()+
    coord_flip()+
    guides(fill = guide_legend(reverse = TRUE))}
  
  arquivo <- paste0('img/bi/', x, 'X', y, '.pdf')
  ggsave(arquivo, dpi = 500)
  return(ax)
}



# Boxplot -----------------------------------------------------------------

uni_boxplot <- function(banco, var, xlab= '', ylab = '', color = '#767F8B', flip = F,capitalize=T){
  
  banco <- banco[banco[var] != '',][var]
  
  if(typeof(banco[3,var]) == 'character')
  {  
    if(capitalize == T){
      prim.maiuscula <- function(x) {
        x <- tolower(x)
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
      }
      
      banco[var] <- sapply(banco[var], FUN = prim.maiuscula)}
  }
  
  ax <- ggplot(banco, aes(x = '', y = .data[[var]])) + 
    stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
    geom_boxplot(fill = color) +
    theme_minimal()+
    labs(x= xlab, y = ylab)
  
  if(flip == T){ax <- ax+coord_flip()}
  
  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)
  return(ax)
}



# Boxplot bivariado -------------------------------------------------------

bi_boxplot <- function(banco, x, y, xlab= '', ylab = '', color = '#767F8B', flip = F,capitalize = T){
  
  banco <- banco[!is.na(banco[x]) & !is.na(banco[y]),][,c(x,y)]
  
  prim.maiuscula <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  if(capitalize==T){
    if(typeof(banco[3,x]) == 'character') {banco[x] <- sapply(banco[x], FUN = prim.maiuscula)}
    if(typeof(banco[3,y]) == 'character') {banco[y] <- sapply(banco[y], FUN = prim.maiuscula)}}

    ax <- ggplot(banco, aes(x = .data[[x]], y = .data[[y]])) + 
      stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
      geom_boxplot(fill = color) +
      theme_minimal()+
      labs(x= xlab, y = ylab)
  
  if(flip == T){     
    ax <- ax + coord_flip() }
    
  arquivo <- paste0('img/bi/', x, 'X', y, '.pdf')
  ggsave(arquivo)
  return(ax)
}


# Histograma --------------------------------------------------------------

histograma <- function(banco, var, xlab= '', ylab = 'Frequência', color = '#767F8B', media = T){
  
  banco <- banco[banco[var] != '',][var]
  
  ax <- ggplot(banco, aes(x = .data[[var]])) + 
          geom_histogram(col = 'black',
                         fill = color,
                         alpha = 0.9)+
          theme_minimal()+
          labs(x= xlab, y = ylab) 

  if(media == T){
    ax <- ax +
          geom_vline(xintercept = mean(banco[[var]], na.rm = T),
                     color = 'red', linetype = 'dashed', lwd = 1.2)
  }

  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)
  
  return(ax)
}

# Scatterplot -------------------------------------------------------------

scatterplot <- function(banco, x, y, xlab = '', ylab = '', reg = F, size = 1.7,  color = '#000000')
{
  
  formato <- theme(                                                       
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, vjust = 0.5, angle= 0),
    axis.title.x = element_text(size = 12, vjust = -0.2),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )
  
  ax <- ggplot(banco, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = 0.9, size = size)+
    labs(x = xlab, y = ylab)+
    theme_minimal()+
    formato
  if(reg){ ax <- ax + geom_smooth(method = 'lm', se = F, col = 'red')}
  return(ax)
  
}



