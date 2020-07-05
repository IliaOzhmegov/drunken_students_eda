{ # Setup ----------------------------------------------------------------------
  # library("ggpubr")
  library(tidyverse)
  library(gridExtra)
  
  grades_colours <- c("#00d27f","#adff00","#f9d62e","#fc913a","#ff4e50")
}

# Just grades dummy research ---------------------------------------------------
full_df %>%
    ggplot(aes(x=G3_d, fill=G1)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>% ggplot(aes(x=G1)) + geom_bar()
full_df %>% ggplot(aes(x=G2)) + geom_bar()

full_df %>%
    ggplot(aes(x=G3, fill=G2)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>%
    ggplot(aes(x=G2, fill=G1)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>%
    ggplot() +
    geom_bar(aes(x=G3_d, fill=G1), position="dodge") +
    scale_fill_manual(values=grades_colours) 

full_df %>% 
    group_by(G3_d) %>% 
    dplyr::summarise(n = n())

full_df %>% 
    select(G3) %>% 
    group_by(G3) %>% 
    dplyr::summarise(n=n())

full_df %>% 
    ggplot(aes(x=G3)) + geom_bar()

full_df %>% 
    ggplot(aes(x=Dalc, y=G3)) + geom_boxplot()

# levels(full_df$Dalc)
# str(full_df$Dalc)
# summary(full_df$Dalc)


# Does Alcohol Affect Success? -------------------------------------------------
full_df %>% 
    ggplot(aes(x=Salc)) + 
    geom_bar(position="dodge") 

full_df %>% 
    ggplot(aes(x=Walc)) + 
    geom_bar(position="dodge") 

# full_df %>% 
#     ggplot(aes(x=Walc, fill=Dalc)) + 
#     geom_bar(position="dodge") 

# Alc consumption during Weekend and weekday
# Breaking bads during weekend

{ # important one: Alc consumpution is higher at Weekends
    p1 <- full_df %>% 
        ggplot(aes(x=Dalc, fill=G3)) + geom_bar(position="dodge") +
        scale_fill_manual(values=grades_colours)
    
    p2 <- full_df %>% 
        ggplot(aes(x=Walc, fill=G3)) + geom_bar(position="dodge") +
        scale_fill_manual(values=grades_colours)
    
    p3 <- full_df %>% 
          ggplot(aes(x= Dalc, y=Walc)) +
          theme(legend.position="none") +
          geom_jitter(aes(colour = Dalc)) 
    
    grid.arrange(p1, p2, p3, nrow = 2, ncol=2)
}

# full_df %>% 
#     ggplot(aes(x= Dalc, y=G3)) +
#     geom_jitter(aes(colour = Walc)) 

{ # important one: there is no strong dependce between alc cons and success
    p21 <- full_df %>%  
        ggplot(aes(x= Dalc, y=G3, fill=Dalc)) +
        geom_violin() +
        scale_fill_manual(values=grades_colours)
    
    p22 <- full_df %>% 
        ggplot(aes(x= Walc, y=G3, fill=Walc)) +
        geom_violin() +
        scale_fill_manual(values=grades_colours)
    grid.arrange(p21, p22, nrow = 2)
}

{ # comparison alc consumption during weekdays and weekends
    xlab = "Weekday alcohol consumption"
    d1 <- full_df %>% 
        ggplot(aes(x=Dalc, y=G1, fill=Dalc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("First period grade")
    
    d2 <- full_df %>% 
        ggplot(aes(x=Dalc, y=G2, fill=Dalc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("Second period grade")
    
    d3 <- full_df %>% 
        ggplot(aes(x=Dalc, y=G3, fill=Dalc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("Final period grade")
    
    xlab = "Weekend alcohol consumption"
    e1 <- full_df %>% 
        ggplot(aes(x=Walc, y=G1, fill=Walc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("First period grade")
    
    e2 <- full_df %>% 
        ggplot(aes(x=Walc, y=G2, fill=Walc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("Second period grade")
    
    e3 <- full_df %>% 
        ggplot(aes(x=Walc, y=G3, fill=Walc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        scale_fill_manual(values=grades_colours) +
        xlab(xlab) +
        ylab("Grade") +
        ggtitle("Final period grade")
    
    grid.arrange(d1, d2, d3, e1, e2, e3, ncol=3, nrow=2)
    
    
    full_df %>% 
        ggplot(aes(x=Salc, y=G3, fill=Salc)) +
        geom_boxplot() +
        theme_bw() +
        theme(legend.position="none") +
        # scale_fill_manual(values=grades_colours) +
        xlab("Alcohol Consumption") +
        ylab("Grade") +
        ggtitle("Final period grade")
}

{ # What's classes can be found in the dataset? 
  # (Well-educated alcos or Uneducated alcos)
  full_df %>% 
    ggplot(aes(x=Salc, G3_d)) +
    geom_jitter(aes(colour = Salc)) + 
    geom_hline(yintercept='C',      color = "black") + 
    geom_vline(xintercept='Medium', color = "black")  
    
}

{ # correlation matrix
  # corr.df = cor(full_df, method = c("spearman"))
  
  # how to get correlation
  int_df <- lapply(full_df, as.integer)
  n = length(int_df)
  
  c = rep(0, n)
  for(i in 1:n) c[i] = cor(int_df$G3, int_df[[i]]) %>% abs()
  
  
  
  # plot(c)
  ffff = data.frame(correlation=c, name=colnames(full_df)) 
  
  # decr = c %>% order(decreasing = TRUE)
  # ffff %<>% mutate(
  #   correlation = correlation[decr],
  #   name        = name[decr]
  # )
  
  ffff %>% filter(!str_detect(name, "^G")) %>% 
    ggplot(aes(x=correlation, y=reorder(name, correlation))) + geom_bar(stat="identity")
  
  full_df %>% 
    ggplot(aes(x=G3, y=failures)) + geom_bar(stat="identity")
  
  full_df %>% 
    ggplot(aes(x=G3, y=failures)) + 
    geom_jitter(aes(colour = higher))
  
  # full_df %>% 
  #   ggplot(aes(x=G3, y=higher)) + geom_jitter()
  
  full_df %>% 
    ggplot(aes(x=G3_d, fill=higher)) + geom_bar(position="dodge")
  # lm(G3~failures + I(failures^2), data = full_df) %>% summary()
  
  # colnames(full_df)
  
  # cor(int_df[[1]], int_df[[1]])
  
  # pairs(full_df[1:4])
  
}

# -- 
full_df %>% 
    ggplot(aes(x=Walc, y=G3, fill=Walc)) +
    geom_boxplot() +
    scale_fill_manual(values=grades_colours)

# - 

full_df %>% 
    ggplot(aes(x=Dalc, fill=G3_d)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>% 
    ggplot(aes(x=Walc, fill=G3)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>% 
    ggplot(aes(x=G3, fill=Dalc)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours)

full_df %>% 
    ggplot(aes(x=G3, fill=Dalc)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours) 
    
full_df %>%  # importan one
    ggplot(aes(x=G3_d, fill=Dalc)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours) +
    scale_y_continuous(trans = 'log10')

full_df %>%  # importan one
    ggplot(aes(x=G3_d, fill=Walc)) + 
    geom_bar(position="dodge") +
    scale_fill_manual(values=grades_colours) +
    scale_y_continuous(trans = 'log10')

{ # a try to visualize three variable at the same time
  full_df %>% 
    ggplot(aes(x=age %>% as.factor(),  y=G3, fill=gender)) +
    geom_dotplot(binaxis = "y", 
                 stackdir = "center", 
                 position = "dodge",
                 dotsize = 0.2, binwidth = 0.5)
    
  full_df %>% 
    ggplot(aes(x=age %>% as.factor(),  y=G3, fill=gender)) +
    geom_violin(position=position_dodge(1))
    
  full_df %>% 
    group_by(age, Dalc) %>% 
    summarise(
      Walc_n = n(),
      G3_median = median(G3),
    ) %>% 
    ggplot(aes(x=age,  y=G3_median, color=Dalc)) +
    geom_line()
  
  
  { # research over facet_grid
    library(RColorBrewer)
    colours = brewer.pal(9, 'RdYlGn')[9:1]
    
    full_df %>% 
      ggplot(aes(y=freetime, x=Salc, color=Salc, group=freetime)) +
      geom_count() + 
      scale_size(range = c(3, 20)) +
      scale_color_manual(values=colours) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.spacing.x=unit(0.5, "lines")) +
      facet_grid(. ~ goout, switch="both") +
      labs(x = "Goout",
           size = "")
    }
  
    
    
    geom_jitter(aes(colour = Salc))
  
    geom_dotplot(binaxis = "y", 
                 stackdir = "center", 
                 position = "dodge",
                 dotsize = 0.09, binwidth = 0.5)
  
  
}






