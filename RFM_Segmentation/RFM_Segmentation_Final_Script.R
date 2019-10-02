# Load libraries ----
library(tidyverse)
library(lubridate)
library(readr)
library(skimr)
library(broom)
library(fpc)
library(scales)
library(ggrepel)
library(plotly)



# Importing Preclensed Data File ----
orders_tbl <- 
   read_rds("00_data/orders_tbl.rds")

# I've assembled the various data feeds here: 
# (https://github.com/DiegoUsaiUK/Loading_Merging_and_Joining_Datasets).


# create analysis dataset ----
customers_tbl <- 
   orders_tbl %>% 
  
   # assume a cut-off date of 30-June-2007 and create a recency variable 
   mutate(days_since = as.double(
     ceiling(
       difftime(
          time1 = "2007-06-30",
          time2 = orders_tbl$order_date, 
          units = "days")))
          ) %>%
   
   # select last 2 full years 
   filter(order_date <= "2007-06-30") %>% 
   
   # create analysis variables
   group_by(retailer_code) %>%
   summarise(
      recency = min(days_since),
      frequency = n(),
      # average sales
      avg_amount = mean(revenue),
      # total sales
      tot_amount = sum(revenue),
      # number of orders
      order_count = length(unique(order_date))
      ) %>%
   # average order value
   mutate(avg_order_val = tot_amount / order_count) %>% 
   ungroup()

### Single Variable Exploration ----

#### Recency

summary(customers_tbl$recency)

# Plot
(customers_tbl %>% 
      ggplot(aes(x = recency)) +
      geom_histogram(bins = 100, fill = "#E69F00", colour = "red") +
      labs(x = "", 
           y = "", 
           title = "Days since last order") + 
      coord_cartesian(xlim = c(0, 400)) +
      scale_x_continuous(breaks = seq(0, 400, 100)) +
      theme_light()
) %>%
   ggplotly()


#### Frequency


summary(customers_tbl$frequency)

# Plot
(customers_tbl %>% 
      ggplot(aes(x = frequency)) +
      geom_histogram(bins = 50, fill = "steelblue", colour = "blue") +
      labs(x = "", 
           y = "",
           title = "Purchase frequency") + 
      coord_cartesian(xlim = c(0, 9000)) +
      scale_x_continuous(breaks = seq(0, 9000, 1000)) +
      theme_light()
) %>%
   ggplotly()


#### Total and Average Sales

# stacked plotly plots
a <- 
   customers_tbl %>%
   ggplot(aes(x = tot_amount)) +
   geom_histogram(bins = 50, fill = "light green", colour = "forest green") +
   labs(x = "", 
        y = "", 
        title = "") +
   scale_x_continuous(
      labels = scales::dollar_format(scale = 1e-6,suffix = "m"),
      breaks = seq(0, 7e+7, 1e+7)) +
   scale_y_continuous(limits = c(0,80)) +
   theme_light() +
   annotate("text", x = 60e+6, y = 70, label = "Total sales per customer")

b <- 
   customers_tbl %>%
   ggplot(aes(x = avg_amount)) +
   geom_histogram(bins = 50, fill = "forest green", colour = "dark green") +
   labs(x = "Average sales per customer", 
        y = "", 
        title = "") +
   scale_x_continuous(
      labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
      breaks = seq(0, 70000, 10000)) +
   scale_y_continuous(limits = c(0, 80)) +
   theme_light() +
   annotate("text", x = 50e+3, y = 70, label = "Average sales per customer")

subplot(a,b, nrows = 2, margin = 0.04)


#### Number of Orders

summary(customers_tbl$order_count)

# Plot
(customers_tbl %>%
      ggplot(aes(x = order_count)) +
      geom_histogram(bins = 60, fill = "firebrick3", colour = "sandybrown") +
      labs(x = "", 
           y = "", 
           title = "Number of Orders per Customer") +
      scale_x_continuous(breaks = seq(0, 300, 50)) +
      theme_light()
) %>% 
   ggplotly()


#### Average Order Value

summary(customers_tbl$avg_order_val)

# Plot
(customers_tbl %>%
      ggplot(aes(x = avg_order_val)) +
      geom_histogram(
         bins = 50,
         fill = "purple", colour = "black") +
      labs(x = "", 
           y = "",
           title = "Average Order Value") +
      scale_x_continuous(
         labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
         breaks = seq(0, 5e+5, 1e+5)) +
      theme_light()
) %>% 
   ggplotly()


### Multiple Variables Exploration ----

# `recency`, `frequency` and `average sales` per customer - frequency to colour-code the points.

# Plot
(customers_tbl %>% 
   ggplot(aes(x = (recency), y = (avg_amount))) + 
   geom_point(aes(colour = frequency)) + 
   scale_colour_gradient(name = "Frequency", 
                         high = "#132B43", 
                         low = "#56B1F7") +
   scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,
                                                     suffix = "k")) +
   labs(x = "Recency", 
        y = "Average Sales",
        title = "") +
   theme_light()
) %>%
  ggplotly()


# log-transformed plot
(customers_tbl %>% 
   ggplot(aes(x = log(recency), y = log(avg_amount))) + 
   geom_point(aes(colour = log(frequency))) + 
   scale_colour_gradient(name = "Log Freq.", 
                         high = "#132B43", 
                         low = "#56B1F7") +
   labs(x = "Log Recency", 
        y = "Log Average Sales",
        title = "") +
   theme_light()
) %>% 
  ggplotly()


## The Analysis  ----

# scale the variables
clust_data <- 
   customers_tbl %>% 
   select(-retailer_code) %>% 
   scale() %>% 
   as_tibble()


# function to calculate `kmeans` for any number of centres 
kmeans_map <- function(centers = centers) {
   set.seed(1975)                   # for reproducibility
   clust_data[,1:3] %>%  
      kmeans(centers = centers, 
             nstart = 100, 
             iter.max = 50)
}

# create a nested tibble to house all the model output
kmeans_map_tbl <- 
   tibble(centers = 1:10) %>%       # create column with centres 
   mutate(k_means = centers %>% 
             map(kmeans_map)) %>%   # iterate `kmeans_map` row-wise to gather 
                                    # kmeans models for each centre in column 2
   
   mutate(glance = k_means %>%      # apply `glance()` row-wise to gather each
             map(glance))           # model’s summary metrics in column 3
                                    



# `scree plot` 
kmeans_map_tbl %>% 
   unnest(glance) %>%                          # unnest the glance column
   select(centers, tot.withinss) %>%         # select centers and tot.withinss
   
   ggplot(aes(x = centers, y = tot.withinss)) + 
   geom_line(colour = 'grey30', size = .8) +
   geom_point(colour = 'green4', size = 3) +
   geom_label_repel(aes(label = centers),
                    colour = 'grey30') +
   labs(title = 'Scree Plot') +
   theme_light()


### Evaluating the Clusters ----

# Plot for 4 clusters
kmeans_4_tbl <- 
   kmeans_map_tbl %>% 
   pull(k_means)  %>%
   pluck(4) %>%               # pluck element 4 
   augment(customers_tbl)     # attach .cluster to the tibble

(kmeans_4_tbl %>% 
   ggplot(aes(x = log(recency), y = log(avg_amount))) + 
   geom_point(aes(colour = .cluster)) + 
   labs(x = "Log Recency", 
        y = "Log Average Sales",
        title = "") +
   theme_light()
) %>% 
   ggplotly()


# Summary table for 4 clusters
options(digits = 2)

kmeans_4_tbl %>% 
   group_by(.cluster) %>% 
   summarise(
      Retailers = n(),
      Recency = median(recency),
      Frequency = median(frequency),
      Avg.Sales = median(avg_amount)
      ) %>% 
   ungroup() %>% 
   mutate(`Retailers(%)` = 100*Retailers / sum(Retailers)) %>% 
   arrange((.cluster)) %>% 
   select(c(1,2,6,3:5)) %>% 
   kable() 



## Alternative Analysis ----

# new features: `average order value`, `orders per customer`and 
# `average sales per customer` 

# Plot
(customers_tbl %>% 
   ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
   geom_point(aes(colour =  log(avg_amount))) + 
   scale_colour_gradient(name = "Avg. Sales", 
                         high = "#132B43", 
                         low = "#56B1F7") +
   labs(x = "Orders per Cust.", 
        y = "Avg Order Value",
        title = "") +
   theme_light()
) %>% 
  ggplotly()
 
# re-run analysis

# function for a set number of centers
kmeans_map_alt <- function(centers = centers) {
   set.seed(1975)                       # for reproducibility
   clust_data[,4:6] %>%                 # select relevant features
      kmeans(centers = centers, 
             nstart = 100, 
             iter.max = 50)
}

# create nested tibble
kmeans_map_tbl_alt <- 
   tibble(centers = 1:10) %>%           # create column with centres 
   mutate(k_means = centers %>% 
             map(kmeans_map_alt)) %>%   # iterate `kmeans_map_alt` row-wise to gather 
   # kmeans models for each centre in column 2
   
   mutate(glance = k_means %>%          # apply `glance()` row-wise to gather each
             map(glance))               # model’s summary metrics in column 3


# scree plot
kmeans_map_tbl_alt %>% 
   unnest(glance) %>%                         # unnest the glance column
   select(centers, tot.withinss) %>%         # select centers and tot.withinss
   
   ggplot(aes(x = centers, y = tot.withinss)) + 
   geom_line(colour = 'grey30', size = .8) +
   geom_point(colour = 'green4', size = 3) +
   geom_label_repel(aes(label = centers),
                    colour = 'grey30') +
   labs(title = 'Scree Plot') +
   theme_light()


### Evaluating the Clusters ----

# Plot for 4 clusters
kmeans_4_tbl_alt <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(4) %>%               # pluck element 4 
   augment(customers_tbl)     # attach .cluster to the tibble


(kmeans_4_tbl_alt %>% 
      ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
      geom_point(aes(colour = .cluster)) +  
      labs(x = "Number of Orders", 
           y = "Average Order Value",
           title = "Segmentation on 4 clusters") +
      theme_light()
) %>% 
   ggplotly()


# Summary table 4 clusters
options(digits = 2)

kmeans_4_tbl_alt %>%
group_by(.cluster) %>% 
   summarise(
      Retailer = n(),
      Avg.Sales = median(avg_amount),
      Orders = median(order_count),
      Avg.Order.Val = median(avg_order_val),
      Recency = median(recency),
      Frequency = median(frequency)
      ) %>% 
   ungroup() %>% 
   mutate(`Retailers(%)` = 100 * Retailer / sum(Retailer)) %>% 
   arrange((.cluster)) %>% 
   select(c(1,2,8, 3:7)) %>% 
   kable()



# Stacked plot comparing 4-, 5-, 6- and 7-cluster configuration
c <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(4) %>%                        
   augment(customers_tbl) %>% 
   ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
   geom_point(aes(colour = .cluster)) +  
   # scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,
   #                                                suffix = "k")) +
   theme(legend.position = "none") +
   labs(x = "Number of Orders", 
        y = "Avg. Order Value",
        title = "") +
   theme_light()

d <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(5) %>%                        
   augment(customers_tbl) %>% 
   ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
   geom_point(aes(colour = .cluster)) +  
   # scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,
   #                                                suffix = "k")) +
   theme(legend.position = "none") +
   labs(x = "Number of Orders", 
        y = "Avg. Order Value",
        title = "") +
   theme_light()

e <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(6) %>%                        
   augment(customers_tbl) %>% 
   ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
   geom_point(aes(colour = .cluster)) +  
   # scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,
   #                                                suffix = "k")) +
   theme(legend.position = "none") +
   labs(x = "Number of Orders", 
        y = "Avg. Order Value",
        title = "") +
   theme_light()

f <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(7) %>%                        
   augment(customers_tbl) %>% 
   ggplot(aes(x = log(order_count), y = log(avg_order_val))) + 
   geom_point(aes(colour = .cluster)) +  
   # scale_y_continuous(labels = scales::dollar_format(scale = 1e-3,
   #                                                suffix = "k")) +
   theme(legend.position = "none") +
   labs(x = "Number of Orders", 
        y = "Avg. Order Value",
        title = "") +
   theme_light()

subplot(c, d, e, f, nrows = 2, 
        shareX = T,
        shareY = T,
        margin = 0.04) 


## Principal Components Analysis ----

# create PCA object
pca_obj <- 
   customers_tbl[,5:7] %>% 
   prcomp(center = TRUE, 
          scale. = TRUE)


summary(pca_obj)

# Plot - Percentage of variance explained
(data.frame(summary(pca_obj)$importance) %>%    # extract importance as a dataframe
      rownames_to_column() %>%                     # get metrics names in a column
      pivot_longer(c(2:4),                         
                   names_to = "PC", 
                   values_to = "value") %>%        # using tidyr::pivot_longer, the new gather 
      
      filter(rowname == 'Proportion of Variance') %>%
      ggplot(aes(x = PC, y = value)) +
      geom_col(aes(fill =  value)) +
      scale_fill_gradient(high = "grey5", low = "grey50") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Principal Component", 
           y = "Percentage of Variance Explained",
           title = "Variance Explained by Principal Component")
) %>% 
   ggplotly()


### PCA visualisation - 4 clusters ----

# I extract the PCA from `pca_obj` and join the PCs co-ordinate contained in `x` 
# with the `retailer` information from the original `customer_tbl` set

pca_tbl <- 
   pca_obj$x %>%                 # extract "x", which contains the PCs co-ordinates
   as_tibble() %>%               # change to a tibble
   bind_cols(customers_tbl %>%   # append retailer_code, my primary key
                select(retailer_code))


# `pluck` the 4th element from `kmeans_map_tbl_alt`, 
# attach the cluster info to it, and `left_join` by retailer_code 

km_pca_4_tbl <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(4) %>%                  # pluck element 4 
   augment(customers_tbl) %>%    # attach .cluster to the tibble 
   left_join(pca_tbl,            # left_join by my primary key, retailer_code 
             by = 'retailer_code')

# plot
(km_pca_4_tbl %>% 
      mutate(label_text = str_glue('Retailer: {retailer_code}
                                  Cluster: {.cluster}
                                  Recency: {recency}
                                  Frequency: {frequency}
                                  Avg Value: {avg_amount}')) %>%
      ggplot(aes(x = PC1, y = PC2, colour = .cluster)) +
      geom_point(aes(text = label_text, shape = .cluster)) +
      labs(title    = 'K-Means 4 Clusters Against First Two Principal Components',
           caption  = "") +
      theme(legend.position = 'none') +
      theme_light()
) %>% 
   ggplotly(tooltip = "label_text")


# Summary table for 4 clusters
options(digits = 2)

kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(4) %>%                        
   augment(customers_tbl) %>%
   group_by(.cluster) %>% 
   summarise(
      Retailers = n(),
      Avg.Sales = median(avg_amount),
      Orders = median(order_count),
      Avg.Order.Val = median(avg_order_val),
      `Tot.Sales(m)` = sum(tot_amount/1e+6)
   ) %>% 
   ungroup() %>% 
   mutate(`Tot.Sales(%)` = 100 * `Tot.Sales(m)` / sum(`Tot.Sales(m)`),
          `Retailers(%)` = 100*Retailers / sum(Retailers))  %>% 
   select(c(1, 2, 8, 3:7)) %>%
   kable()


### PCA visualisation - 6 clusters ----

# `pluck` the 6th element from `kmeans_map_tbl_alt`, 
# attach the cluster info to it, and `left_join` by retailer_code
options(digits = 2)

kmeans_6_tbl <- 
   kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(6) %>%                  # pluck element 6 
   augment(customers_tbl) %>%    # attach .cluster to the tibble 
   left_join(pca_tbl,            # left_join by retailer_code
             by = 'retailer_code')

# plot
(kmeans_6_tbl %>% 
   mutate(label_text = str_glue('Retailer: {retailer_code}
Cluster: {.cluster}
Recency: {recency}
Frequency: {frequency}
Avg Value: {avg_amount}')) %>%
   ggplot(aes(x = PC1, y = PC2, colour = .cluster)) +
   geom_point(aes(text = label_text, shape = .cluster)) +
   labs(title    = 'K-Means 6 Clusters Against First Two Principal Components',
        caption  = "") +
   theme(legend.position = 'none') +
   theme_light()
) %>% 
   ggplotly(tooltip = "label_text")


# Summary table for 6 clusters
options(digits = 2)

kmeans_map_tbl_alt %>% 
   pull(k_means) %>%
   pluck(6) %>%                        
   augment(customers_tbl) %>%
   group_by(.cluster) %>% 
   summarise(
      Retailers = n(),
      Avg.Sales = median(avg_amount),
      Orders = median(order_count),
      Avg.Order.Val = median(avg_order_val),
      `Tot.Sales(m)` = sum(tot_amount/1e+6)
      ) %>% 
   ungroup() %>% 
   mutate(`Tot.Sales(%)` = 100 * `Tot.Sales(m)` / sum(`Tot.Sales(m)`),
          `Retailers(%)` = 100*Retailers / sum(Retailers))  %>% 
   select(c(1, 2, 8, 3:7)) %>%
   kable()

## Clusterboot Evaluation ----

kmeans_boot100 <-
   clusterboot(
      clust_data[,4:6],
      B = 50,                    # number of resampling runs
      bootmethod = "boot",       # resampling method: nonparametric bootstrap
      clustermethod = kmeansCBI, # clustering method: k-means 
      k = 7,                     # number of clusters 
      seed = 1975)               # for reproducibility

# results to data frame
bootMean_df <- 
   data.frame(cluster = 1:7, 
              bootMeans = kmeans_boot100$bootmean)

# visualise tests output with a simple chart
(bootMean_df %>%
      ggplot(aes(x = cluster, y = bootMeans)) +
      geom_point(colour = 'green4', size = 4) +
      geom_hline(yintercept = c(0.6, 0.8)) +
      theme_light() +
      labs(x = "Stability",
           title = "Clusterboot Stability Evaluation") +
      theme(legend.position = "none")) %>% ggplotly()

# Remember that values:
   
# - __above 0.8__ (segment 2, 3 and 5) indicates highly stable clusters
# - __between 0.6 and 0.75__ (segments 1, 4 and 6) signal a acceptable degree of stability
# - __below 0.6__ (segment 7) should be considered unstable
