library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)

pct_format = scales::percent_format(accuracy = .1)



# Gráfico de barras univariado --------------------------------------------

uni_bar <- function(banco, var, ylab= 'Frequência', xlab = '', color = '#767F8B', label_size = 0.25, order = NULL, flip = F){
  
  banco <- banco[banco[var] != '',][var]
  
  if(typeof(banco[3,var]) == 'character')
  {  
    prim.maiuscula <- function(x) {
      x <- tolower(x)
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    
    banco[var] <- sapply(banco[var], FUN = prim.maiuscula)
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
  
  print(ax)
  
  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)

  
}

# Gráfico de barras bivariado ---------------------------------------------

bi_bar <- function(banco, x, y, ylab= 'Frequência', xlab = '', label_fill = '', color = c('#767F8B', '#B3B7B8'), label_size = 0.25, order = NULL, flip = F){
  
  banco <- banco[banco[x] != '' & banco[y] != '',][,c(x,y)]
  prim.maiuscula <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  if(typeof(banco[3,x]) == 'character') {banco[x] <- sapply(banco[x], FUN = prim.maiuscula)}
  if(typeof(banco[3,y]) == 'character') {banco[y] <- sapply(banco[y], FUN = prim.maiuscula)}
  
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
      ), group = y), position = position_dodge2(width = 0.9, ),
      size=2.5,hjust= 0.5, col = "black", fill='white')+
    scale_fill_manual(values=color,)+
    theme_minimal()+
    coord_flip()+
    guides(fill = guide_legend(reverse = TRUE))}
  
  print(ax)
  
  arquivo <- paste0('img/bi/', x, 'X', y, '.pdf')
  ggsave(arquivo, dpi = 500)
  
  
}


# Boxplot -----------------------------------------------------------------

uni_boxplot <- function(banco, var, xlab= '', ylab = '', color = '#767F8B', flip = F){
  
  banco <- banco[banco[var] != '',][var]
  
  if(typeof(banco[3,var]) == 'character')
  {  
    prim.maiuscula <- function(x) {
      x <- tolower(x)
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    
    banco[var] <- sapply(banco[var], FUN = prim.maiuscula)
  }
  
  if(flip == F)
  { 
    ax <- ggplot(banco, aes(x = '', y = .data[[var]])) + 
    stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
    geom_boxplot(fill = color) +
    theme_minimal()+
    labs(x= xlab, y = ylab) }
    
  else if(flip == T)
  {     
    ax <- ggplot(banco, aes(x = '', y = .data[[var]])) + 
    stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
    geom_boxplot(fill = color) +
    theme_minimal()+
    labs(x= xlab, y = ylab) +
    coord_flip() }
  
  print(ax)
  
  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)
  
}



# Boxplot bivariado -------------------------------------------------------

bi_boxplot <- function(banco, x, y, xlab= '', ylab = '', color = '#767F8B', flip = F){
  
  banco <- banco[!is.na(banco[x]) & !is.na(banco[y]),][,c(x,y)]
  
  prim.maiuscula <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  if(typeof(banco[3,x]) == 'character') {banco[x] <- sapply(banco[x], FUN = prim.maiuscula)}
  if(typeof(banco[3,y]) == 'character') {banco[y] <- sapply(banco[y], FUN = prim.maiuscula)}
  
  if(flip == F)
  { 
    ax <- ggplot(banco, aes(x = .data[[x]], y = .data[[y]])) + 
      stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
      geom_boxplot(fill = color) +
      theme_minimal()+
      labs(x= xlab, y = ylab) }
  
  else if(flip == T)
  {     
    ax <- ggplot(banco, aes(x = .data[[x]], y = .data[[y]])) + 
      stat_boxplot(geom='errorbar', linetype=1, width=0.25)+
      geom_boxplot(fill = color) +
      theme_minimal()+
      labs(x= xlab, y = ylab) +
      coord_flip() }
  
  print(ax)
  
  arquivo <- paste0('img/bi/', x, 'X', y, '.pdf')
  ggsave(arquivo)
  
}


# Histograma --------------------------------------------------------------

histograma <- function(banco, var, xlab= '', ylab = 'Frequência', color = '#767F8B', media = T){
  
  banco <- banco[banco[var] != '',][var]
  
  
  if(media == T){
    ax <- ggplot(banco, aes(x = .data[[var]])) + 
      geom_histogram(col = 'black',
                     fill = color,
                     alpha = 0.9)+
      geom_vline(xintercept = mean(banco[[var]], na.rm = T),
                 color = 'red', linetype = 'dashed', lwd = 1.2)+
      theme_minimal()+
      labs(x= xlab, y = ylab) 
  }
  else if(media == F){
    ax <- ggplot(banco, aes(x = .data[[var]])) + 
      geom_histogram(col = 'black',
                     fill = color,
                     alpha = 0.9)+
      theme_minimal()+
      labs(x= xlab, y = ylab) 
  }
    
  print(ax)
  
  arquivo <- paste0('img/uni/', var, '.pdf')
  ggsave(arquivo)
  
}



bi_bar2 <- function(banco, x, y, ylab= 'Frequência', xlab = '', label_fill = '', color = c('#767F8B', '#B3B7B8'), label_size = 0.25, flip = F,order = NULL, order_fill = NULL){
  
  banco <- banco[banco[x] != '' & banco[y] != '',][,c(x,y)]
  prim.maiuscula <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  if(typeof(banco[3,x]) == 'character') {banco[x] <- sapply(banco[x], FUN = prim.maiuscula)}
  if(typeof(banco[3,y]) == 'character') {banco[y] <- sapply(banco[y], FUN = prim.maiuscula)}
  
  if(!is.null(order))
  {banco[x] <- fct_relevel(factor((banco[[x]]), levels=order))}
  
  if(!is.null(order_fill))
  {banco[y] <- fct_relevel(factor((banco[[y]]), levels=order_fill))}
  
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
  {     ax <- ggplot(banco, aes(x = .data[[x]], y = Freq, fill =.data[[y]])) + 
    geom_bar( stat='identity', position =  "dodge") + 
    theme_minimal()+
    labs(x= xlab, y = 'Frequência', fill = label_fill) + 
    ylim(0.0, max(banco$Freq)*1.05)+
    geom_label(aes(
      label = sprintf(
        '%d (%s)',
        Freq,
        pct_format((Freq / sum(Freq)))
      ), group = y), position = position_dodge2(width = 0.9, ),
      size=2.5,hjust= 0.5, col = "black", fill='white')+
    scale_fill_manual(values=color,)+
    theme_minimal()+
    coord_flip()+
    guides(fill = guide_legend(reverse = TRUE))}
  
  print(ax)
  
  arquivo <- paste0('img/bi/', x, 'X', y, '.pdf')
  ggsave(arquivo)
  
  
}
