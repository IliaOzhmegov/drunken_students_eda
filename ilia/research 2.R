{ # Setup ----------------------------------------------------------------------
  # library("ggpubr")
  library(gridExtra)
  library(ggmosaic)
  
}

{ # Let's just have a look at alcohol consumption during weekDAY and weekEND
  # result: during weekEND consumption is higher
  
  mdf <- reshape2::melt(full_df[,c('Dalc','Walc')],id.vars = 0)
  mdf %>% 
    group_by(variable, value) %>% 
    summarise(n=n()) %>% 
    mutate(
      value = factor(value, levels = c("Very Low", "Low", "Medium", "High", "Very High")) 
    ) %>% 
    ggplot(aes(x=value, y=n)) +
    geom_bar(aes(fill = variable),stat = "identity", position = "dodge") +
    theme_bw() +
    scale_fill_manual(values=matlab.colors[1:2],
                      name = "Weektype", labels = c("Weekday", "Weekend")) +
    theme(legend.position = c(0.9, 0.9), legend.box = "vertical") +
    labs(y = "Number of students", x = "Alcohol consumption", 
         title = "Comparison alcohol consumption during weekend and weekday")
}

{ # Though basically we have categorical data we still want to have a look at
  # correlation between final grade and all others variables
  # result: in general ther is a poor dependce between variables and 
  # final grade, here it is important to remind that all variable are categorical
  # but `failures` and absences (check it out again later)
  
  int_df <- lapply(full_df, as.integer)
  n = length(int_df)
  
  c = rep(0, n)
  for(i in 1:n) c[i] = cor(int_df$G3, int_df[[i]]) %>% abs()

  cor_df = data.frame(correlation=c, name=colnames(full_df)) 
  
  cor_df %>% 
    filter(!str_detect(name, "^G")) %>% 
    ggplot(aes(x=correlation, y=reorder(name, correlation))) + 
    geom_bar(stat="identity", aes(fill=str_detect(name, "alc$"))) +
    scale_fill_manual(values = matlab.colors[1:2]) +
    theme_bw() +
    theme(legend.position="none") +
    labs(y = "Variable", x = "|Correlation|", title = "Final Grade Correlation")
  
}

{ # Simple presentaion of grades before diving deep
  ddf <- full_df %>% 
    mutate(
      success = ifelse(G3_d == 'F' | G3_d == 'D', 'No', 'Yes'),
      G3_d = factor(G3_d, levels = c('F', 'D', 'C', 'B', 'A'))
      )  
    
  p1 <- ddf %>% 
    ggplot(aes(x=G3_d, fill=success)) +
    geom_bar(position = "dodge") +
    theme_bw() +
    scale_fill_manual(values=matlab.colors[2:1]) +
    labs(y = "Number of students", 
         x = "Final Grade (Descrete)", 
         title = "Final Grade Descrete Distribution")
    
  p2 <- ddf %>% 
    ggplot(aes(x=success, fill=success)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values=matlab.colors[2:1]) +
    theme_bw() +
    theme(legend.position="none") +
    scale_x_discrete(breaks=c("No","Yes"),
                     labels=c("Low", "High")) +
    labs(y = "Number of students", 
         x = "Final Grade (Binary)", 
         title = "Final Grade Binary Distribution")
  
  grid.arrange(p1, p2, nrow=1)

  # Now let's have a look directrly at dependence between 
  # Grades and Alcohol consumption
  # result: decent grades are much less frequent among high alcohol consumption
  # also there is no dependence between low alcohol consumption and 
  # decent grades
  
  class_df = full_df %>% 
    mutate(
      s_class = ifelse(G3 < 12, 1, 2),
      s_class = s_class + ifelse(as.numeric(Salc) < 5, 0, 2),
      G3_d = factor(G3_d, levels = c('F', 'D', 'C', 'B', 'A'))
      ) 
  
  p3 <- class_df %>% 
    ggplot(aes(x=Salc, G3_d)) +
    geom_jitter(aes(colour = as.factor(s_class))) + 
    scale_color_manual(values=matlab.colors[c(2, 3, 4, 1)]) +
    theme_bw() +
    theme(legend.position="none") +
    labs(y = "Final Grade (Descrete)", 
         x = "Alcohol consumption", 
         title = "Final period grade")
  
  p4 <- class_df %>% 
    mutate(
      failure   = ifelse( s_class %% 2 == 1, 'High', 'Low'),
      addiction = ifelse( s_class > 3, 'High', 'Low'),
      failure   = factor(failure,   levels = c('Low', 'High')),
      addiction = factor(addiction, levels = c('Low', 'High'))
    ) %>% 
    ggplot() +
    geom_mosaic(aes(x=product(failure, addiction), fill=failure), na.rm = TRUE) +
    scale_fill_manual(values=matlab.colors[2:1]) +
    theme_bw() +
    theme(legend.position="none") +
    labs(x = "The amount of alcohol consumption",
         y = "Final Grade (Binary)",
         title = "Final period grade")
           
  grid.arrange(p3, p4, nrow = 1)
}

# pure research ----------------------------------------------------------------

{ # how does age and gender affect on success?
  # result: the majority of students are in C and D areas, there is no difference
  # between males and females, but after the age of 18 the distribution 
  # is completely different and we do not know why
  
  { # custom split violin
    GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                               draw_group = function(self, data, ..., draw_quantiles = NULL) {
                                 data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                                 grp <- data[1, "group"]
                                 newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                                 newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                                 newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                                 
                                 if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                   stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                             1))
                                   quantiles  <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                   aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                   aesthetics$alpha <- rep(1, nrow(quantiles))
                                   both <- cbind(quantiles, aesthetics)
                                   quantile_grob <- GeomPath$draw_panel(both, ...)
                                   ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                                 }
                                 else {
                                   ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                                 }
                               })
    
    geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                                  draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                                  show.legend = NA, inherit.aes = TRUE) {
      layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
            position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
            params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
    }
  }
  
  { # custom split violin 2
    GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                               draw_group = function(self, data, ..., draw_quantiles = NULL) {
                                 # Original function by Jan Gleixner (@jan-glx)
                                 # Adjustments by Wouter van der Bijl (@Axeman)
                                 data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                                 grp <- data[1, "group"]
                                 newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                                 newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                                 newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                                 if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                   stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                                   quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                                   aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                   aesthetics$alpha <- rep(1, nrow(quantiles))
                                   both <- cbind(quantiles, aesthetics)
                                   quantile_grob <- GeomPath$draw_panel(both, ...)
                                   ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                                 }
                                 else {
                                   ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                                 }
                               }
    )
    
    create_quantile_segment_frame <- function(data, draw_quantiles, split = FALSE, grp = NULL) {
      dens <- cumsum(data$density) / sum(data$density)
      ecdf <- stats::approxfun(dens, data$y)
      ys <- ecdf(draw_quantiles)
      violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
      violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
      violin.xs <- (stats::approxfun(data$y, data$x))(ys)
      if (grp %% 2 == 0) {
        data.frame(
          x = ggplot2:::interleave(violin.xs, violin.xmaxvs),
          y = rep(ys, each = 2), group = rep(ys, each = 2)
        )
      } else {
        data.frame(
          x = ggplot2:::interleave(violin.xminvs, violin.xs),
          y = rep(ys, each = 2), group = rep(ys, each = 2)
        )
      }
    }
    
    geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                                  draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                                  show.legend = NA, inherit.aes = TRUE) {
      layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, 
            show.legend = show.legend, inherit.aes = inherit.aes, 
            params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
    } 
  }
  
  age_min = (full_df$age - 0.5) %>% min() %>% as.factor()
  age_max = (full_df$age + 0.5) %>% max() %>% as.factor()
  
  common_alpha = 0.4
  
  full_df %>% 
    ggplot() +
    annotate("rect",
             xmin = age_min, xmax = age_max,
             ymin = 0,  ymax = 10, fill = brewer.pal(5, 'RdYlGn')[[1]],
             alpha = common_alpha
    ) +
    annotate("rect",
             xmin = age_min, xmax = age_max,
             ymin = 10, ymax = 12, fill = brewer.pal(5, 'RdYlGn')[[2]],
             alpha = common_alpha
    ) +
    annotate("rect",
             xmin = age_min, xmax = age_max,
             ymin = 12, ymax = 14, fill = brewer.pal(5, 'RdYlGn')[[3]],
             alpha = common_alpha
    ) +
    annotate("rect",
             xmin = age_min, xmax = age_max,
             ymin = 14, ymax = 16, fill = brewer.pal(5, 'RdYlGn')[[4]],
             alpha = common_alpha
    ) +
    annotate("rect",
             xmin = age_min, xmax = age_max,
             ymin = 16, ymax = 20, fill = brewer.pal(5, 'RdYlGn')[[5]],
             alpha = common_alpha
    ) +
    geom_split_violin(aes(x=age %>% as.factor(),  y=G3, fill=gender),
                      draw_quantiles = c(0.5)) +
    scale_fill_manual(values=alpha(matlab.colors[2:1], 0.8)) +
    theme_bw() +
    annotate("text", x = "22", y = 5,  label = "F-students\n area") +
    annotate("text", x = "22", y = 11, label = "D-students\n area") +
    annotate("text", x = "22", y = 13, label = "C-students\n area") +
    annotate("text", x = "22", y = 15, label = "B-students\n area") +
    annotate("text", x = "22", y = 18, label = "A-students\n area") +
    labs(x = "Age",
         y = "Final grade",
         title = "Comparison males and females' grades against age")
    
  
    # geom_boxplot(width=0.3)
  
  
}

{ # Let's have a look at dependence of failures and absences
  full_df %>% 
    ggplot(aes(x=G3_d)) +
    geom_bar()
  
  
  full_df %>% 
    ggplot(aes(y=absences, x=studytime)) +
    geom_jitter(aes(colour = as.factor(G3_d))) +
    scale_color_manual(values=colorRampPalette(c("green", "red"))(5)) 
      
    levels(full_df$absences %>% as.factor()) %>% length()
    theme_bw() +
    theme(legend.position="none") +
    labs(y = "Final Grade (Descrete)", 
         x = "Alcohol consumption", 
         title = "Final period grade")
    
    full_df %>% 
      ggplot(aes(y=traveltime, x=G3_d, color=G3_d, group=traveltime)) +
      geom_count() + 
      scale_size(range = c(3, 15)) +
      scale_color_manual(values=colours_5) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.spacing.x=unit(0.5, "lines")) +
      facet_grid(. ~ studytime, switch="both") 
    
    full_df$studytime %>% mean()
    full_df$studytime 
    
    +
      labs(x = "Goout",
           size = "")
}










