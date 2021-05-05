Album of the Year Project
================
Victor Diaz
3/21/2021

## Introduction

The Grammy’s is a yearly awards show that recognizes musicians for their
hard work.Recently I have noticed that many musicians are not recognized
and most of the awards go to popular artists. The following report
investigates this idea to see if it is true. To begin, two datasets were
imported “Metacritic highest scoring albums of the 2010s” and “fan
favorite albums”.The former is data from the metacritic site that
displays the best 50 albums from the best decade and their score
according to the metacritics. The latter dataset is data taken from the
billboard top album chart for the same albums from metacritic dataset
and includes data for peak billboard position, weeks on the billboard
charts, and if the album won a grammy.I also added ten albums that won
Grammys to the datasets to see their impact on the results. I really
enjoy music an thought this project would be really fun to do. I chose
this topic because I recently watched the Grammys.I expect a correlation
between the billboard peak variable and if the album won a Grammy.

## Import/Tidy Data

To begin I imported the datasets to R markdown.

``` r
#allows R to read excel files

library(readxl)
#Upload datasets and rename them simpler
metacritic <- read_excel("metacritic highest scoring albums of the 2010s.xlsx")
billboard <- read_excel("fan favorite best albums.xlsx")
#datasets are already tidy since each variable has its own colulmn and every observation has their own row. No action needed.
```

## Join the Datasets

I then proceded to join the datasets based on the common variable
“Albums”. I called this joined dataset “albums”, which includes all the
variabled from the first two datasets.

``` r
library(tidyverse)
library(tidyr)
#add tidy/dpylr package to R
#Join two datasets by using the common variable "album". Use "inner_join" to do this
albums <-inner_join(metacritic,billboard, by = "Album")
#new dataset created with the first two datasets. No cases were dropped during the merge. I chose this join because I knew all the data had a matches in both datasets.
```

## Summary Statistics

Then I made some summary statisics to better help understand and
organize the data. I found for the group of grammy winners, the average
number of weeks on the charts is 141 while the albums not reconized
averaged 5 weeks.

``` r
#summary statistics
#The following codes use the summary function with different function in order to obtain desired summary statistics.
albums%>%
  summarise(mean_metacriticscore = mean(`Metacritic_Score`))
```

    ## # A tibble: 1 x 1
    ##   mean_metacriticscore
    ##                  <dbl>
    ## 1                 87.9

``` r
#The mean of the variable "metacritic Score" is 87.9.
albums%>%
  summarise(sd_metacriticscore = sd(`Metacritic_Score`))
```

    ## # A tibble: 1 x 1
    ##   sd_metacriticscore
    ##                <dbl>
    ## 1               6.09

``` r
#The standard deviation of metacritic score is 6.09.
albums%>%
  summarise(var_metacriticscore = var(`Metacritic_Score`))
```

    ## # A tibble: 1 x 1
    ##   var_metacriticscore
    ##                 <dbl>
    ## 1                37.1

``` r
#The variance for the metacritic score is 37.1.
albums%>%
  summarise(n_metacritic = n())
```

    ## # A tibble: 1 x 1
    ##   n_metacritic
    ##          <int>
    ## 1           62

``` r
#The number of observations for metacritic is 62.
albums%>%
  arrange(`Metacritic_Score`)
```

    ## # A tibble: 62 x 6
    ##    Album     Artist   Metacritic_Score Billboard_Peak_… Weeks_on_Billboa… Grammy
    ##    <chr>     <chr>               <dbl>            <dbl>             <dbl> <chr> 
    ##  1 Babel     Mumford…               63                1                78 Won   
    ##  2 24K Magic Bruno M…               70                4                41 Won   
    ##  3 Fearless  Taylor …               73                1               116 Won   
    ##  4 25        Adele                  75                1               184 Won   
    ##  5 21        Adele                  76                1               505 Won   
    ##  6 1989      Taylor …               76                1               326 Won   
    ##  7 Morning … Beck                   81                3                34 Won   
    ##  8 When We … Billie …               82                1               102 Won   
    ##  9 Father o… Vampire…               82                1                 8 nomin…
    ## 10 The Subu… Arcade …               87               40                20 Won   
    ## # … with 52 more rows

``` r
#The minimum value for metacritic score is 63.
albums%>%
  arrange(desc(`Metacritic_Score`))
```

    ## # A tibble: 62 x 6
    ##    Album    Artist    Metacritic_Score Billboard_Peak_P… Weeks_on_Billbo… Grammy
    ##    <chr>    <chr>                <dbl>             <dbl>            <dbl> <chr> 
    ##  1 To Pimp… Kendrick…               96                 1               54 nomin…
    ##  2 Ghosteen Nick Cav…               96                 6                1 no    
    ##  3 DAMN     Kendrick…               95                 2               12 nomin…
    ##  4 Black M… D'Angelo                95                 5               15 no    
    ##  5 Skeleto… Nick Cav…               95                13                3 no    
    ##  6 My Beau… Kanye We…               94                 3                5 no    
    ##  7 A Crow … Mount Ee…               93                 0                0 no    
    ##  8 Room 25  Noname                  93                 4                1 no    
    ##  9 Lemonade Beyonce                 92                 2               17 nomin…
    ## 10 Sunbath… Deafheav…               92               112                1 no    
    ## # … with 52 more rows

``` r
#The maximum value for metacritic score is 96.
albums%>%
  summarise(quantile_metacritic = quantile(`Metacritic_Score`))
```

    ## # A tibble: 5 x 1
    ##   quantile_metacritic
    ##                 <dbl>
    ## 1                  63
    ## 2                  88
    ## 3                  89
    ## 4                  91
    ## 5                  96

``` r
#The quantile values for metacritic are 63,88,89,91,and 96.
albums%>%
  summarise(n_distinctmetacritic = n_distinct(`Metacritic_Score`))
```

    ## # A tibble: 1 x 1
    ##   n_distinctmetacritic
    ##                  <int>
    ## 1                   17

``` r
#The distinct values for the metacritic score is 17.
albums%>%
  summarise(cor_metacritic = cor(`Metacritic_Score`,y= `Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   cor_metacritic
    ##            <dbl>
    ## 1          0.133

``` r
#The corrilation between the metacritic score and peak spot on the billboard charts is 0.133.
albums%>%
  summarise(med_metacritic =median(`Metacritic_Score`))
```

    ## # A tibble: 1 x 1
    ##   med_metacritic
    ##            <dbl>
    ## 1             89

``` r
#The median value for metacritic score is 89.

albums%>%
  summarise(mean_peak = mean(`Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   mean_peak
    ##       <dbl>
    ## 1      16.0

``` r
#The mean of the variable "Billboard Peak Position" is 16.
albums%>%
  summarise(sd_peak = sd(`Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   sd_peak
    ##     <dbl>
    ## 1    27.9

``` r
#The standard deviation of peak position is 27.9.
albums%>%
  summarise(var_peak = var(`Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   var_peak
    ##      <dbl>
    ## 1     776.

``` r
#The variance for the peak position is 776.
albums%>%
  summarise(n_peak = n())
```

    ## # A tibble: 1 x 1
    ##   n_peak
    ##    <int>
    ## 1     62

``` r
#The number of observations for peak position is 62.
albums%>%
  select(`Billboard_Peak_Position`)%>%
  arrange(`Billboard_Peak_Position`)
```

    ## # A tibble: 62 x 1
    ##    Billboard_Peak_Position
    ##                      <dbl>
    ##  1                       0
    ##  2                       0
    ##  3                       0
    ##  4                       0
    ##  5                       0
    ##  6                       0
    ##  7                       1
    ##  8                       1
    ##  9                       1
    ## 10                       1
    ## # … with 52 more rows

``` r
#The minimum value for metacritic score is 0 weeks.
albums%>%
  select(`Billboard_Peak_Position`)%>%
  arrange(desc(`Billboard_Peak_Position`))
```

    ## # A tibble: 62 x 1
    ##    Billboard_Peak_Position
    ##                      <dbl>
    ##  1                     142
    ##  2                     112
    ##  3                      88
    ##  4                      60
    ##  5                      55
    ##  6                      54
    ##  7                      52
    ##  8                      50
    ##  9                      40
    ## 10                      40
    ## # … with 52 more rows

``` r
#The maximum value for the peak position is 142 weeks.
albums%>%
  summarise(quantile_peak = quantile(`Billboard_Peak_Position`))
```

    ## # A tibble: 5 x 1
    ##   quantile_peak
    ##           <dbl>
    ## 1           0  
    ## 2           1  
    ## 3           3.5
    ## 4          13  
    ## 5         142

``` r
#The quantile values for peak postion are 0,1,3.5,13,and 142.
albums%>%
  summarise(n_distinctpeak = n_distinct(`Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   n_distinctpeak
    ##            <int>
    ## 1             25

``` r
#The distinct values for the peak position is 25.
albums%>%
  summarise(cor_peak = cor(`Billboard_Peak_Position`,y= `Weeks_on_Billboard_Charts`))
```

    ## # A tibble: 1 x 1
    ##   cor_peak
    ##      <dbl>
    ## 1   -0.207

``` r
#The corrilation between the peak billboard position and weeks on the charts is -0.207.
albums%>%
  summarise(med_peak =median(`Billboard_Peak_Position`))
```

    ## # A tibble: 1 x 1
    ##   med_peak
    ##      <dbl>
    ## 1      3.5

``` r
#The median value for peak position is 3.5.

weeks_stats<-albums%>%
  summarise(mean_weeks = mean(`Weeks_on_Billboard_Charts`),
            sd_weeks= sd(`Weeks_on_Billboard_Charts`),
            var_weeks =var(`Weeks_on_Billboard_Charts`),
            quantile_weeks = quantile(`Weeks_on_Billboard_Charts`),
            n_distinctweeks = n_distinct(`Weeks_on_Billboard_Charts`),
            cor_weeks = cor(`Weeks_on_Billboard_Charts`, y= `Metacritic_Score`),
            med_weeks = median(`Weeks_on_Billboard_Charts`))
#The mean for the variable "weeks on billboard charts" is 33.76, the standard deviation is 80.84, the variation is 6536, the number of entries is 62, the minimum weeks are 0, the maximum weeks is 505, the distinct number of weeks is 32, the corilation with the metacritic score is -0.525, and the median value is 6.
#create a new variable that shows the number of weeks divided by peak position to show the impact of the album.
albums <-albums%>%
  mutate(hit = `Weeks_on_Billboard_Charts`/`Billboard_Peak_Position`)
#Find the statistics for grammy winners, nomineess, and non nominees by grouping by the grammy variable.
albums_grammywin <- albums%>%
 group_by(Grammy )%>%
  summarise(mean_weeks = mean(`Weeks_on_Billboard_Charts`),
            sd_weeks= sd(`Weeks_on_Billboard_Charts`),
            var_weeks =var(`Weeks_on_Billboard_Charts`),
            quantile_weeks = quantile(`Weeks_on_Billboard_Charts`),
            n_distinctweeks = n_distinct(`Weeks_on_Billboard_Charts`),
            cor_weeks = cor(`Weeks_on_Billboard_Charts`, y= `Metacritic_Score`),
            med_weeks = median(`Weeks_on_Billboard_Charts`))
```

## Table of Summary Statistics

``` r
#Create table to organize and display statistics using dash method.

#Statistic Metric Score Peak on Billboard Weeks on Billboard Grammy Winners Grammy Nominess Grammy Losers
#---------------------------------------------------------------------------------------------------------
#mean     |87.9        |16               |33.76             |141.72        |39.29          |5.25         
#sd       |6.09        |27.9             |80.84             |151.24        |27.62          |6.89         
#variance |37.1        |776              |80.84             |22875         |763.23         |47.48        
#n        |62          |62               |62                |12            |7              |43           
#min      |63          |0                |0                 |7             |8              |0            
#max      |96          |142              |505               |505           |80             |28           
#n_distinc|17          |25               |32                |1             |1              |1            
#corilatio|0.133-vspeak|-0.207(vs.weeks) |-5.525(vs.meta)   |NA            |NA             |NA            
#median   |89          |3.5              |6                 |102           |43             |2            
```

## Data Visualization

I then explored the data further by creating some plots to visualize the
data. I created a heat map of correlation numeric variables in the
“album” dataset. The second plot is of artists and the number of Grammys
won. I found that there is a negative correlation between the metacritic
score and the number of weeks an album stays on the billboard charts.I
also noticed that Kendrick Lamar has the highest rated and most albums
on the metacritic dataset.However, he has only been nominated for his
albums and has never won a Grammy for album of the year.

``` r
  #heat map
  # Build a correlation matrix between selected numeric variables
  albums_num <- albums %>%
  select_if(is.numeric) 
cor(albums_num, use = "pairwise.complete.obs")
```

    ##                           Metacritic_Score Billboard_Peak_Position
    ## Metacritic_Score                 1.0000000               0.1327269
    ## Billboard_Peak_Position          0.1327269               1.0000000
    ## Weeks_on_Billboard_Charts       -0.5246963              -0.2072912
    ## hit                             -0.5121445              -0.2213052
    ##                           Weeks_on_Billboard_Charts        hit
    ## Metacritic_Score                         -0.5246963 -0.5121445
    ## Billboard_Peak_Position                  -0.2072912 -0.2213052
    ## Weeks_on_Billboard_Charts                 1.0000000  0.9949761
    ## hit                                       0.9949761  1.0000000

``` r
#Use ggplot to creat graph
cor(albums_num, use = "pairwise.complete.obs") %>%
  as.data.frame %>%
  rownames_to_column %>%
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  ggplot(aes(rowname, other_var, fill=correlation)) +
  geom_tile() +
  scale_fill_gradient2(low="blue",mid="purple",high="red") +
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  labs(title = "Correlation matrix for the dataset albums", x = "variable 1", y = "variable 2")
```

<img src="project-1-bioinfo_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
#produce graph
```

``` r
#Create histogram showing number of grammys won by all artists.
library(tidyverse)
library(ggplot2)
ggplot(albums, aes(x = Artist, fill=Grammy)) +
  geom_bar(position="dodge")+ 
  coord_flip() 
```

<img src="project-1-bioinfo_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
#creates histogram that shows artist and grammys.
```

## Dimesion Reduction

I performed a PCA on the numeric variables(metacritc score, peak,weeks
on Billboard).I found that the metacritic score had the biggest
variation on the data.I then created plots to visualize the PCA.

``` r
# Prepare data for PCA and run PCA.Dont need hit data.Standardize data.
pca <- albums_num %>%
  select(-hit) %>%   
  scale() %>%           
  prcomp()
names(pca)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

``` r
pca
```

    ## Standard deviations (1, .., p=3):
    ## [1] 1.2722120 0.9549953 0.6851719
    ## 
    ## Rotation (n x k) = (3 x 3):
    ##                                  PC1        PC2        PC3
    ## Metacritic_Score          -0.6467770  0.3356639 -0.6848425
    ## Billboard_Peak_Position   -0.3634954 -0.9250654 -0.1101138
    ## Weeks_on_Billboard_Charts  0.6704853 -0.1777180 -0.7203234

``` r
head(pca$x)
```

    ##             PC1       PC2        PC3
    ## [1,] -0.4935386 0.9003782 -1.0291212
    ## [2,] -0.9983281 0.8508497 -0.5766648
    ## [3,] -0.7487569 0.9044067 -0.5464634
    ## [4,] -0.7630218 0.7981912 -0.5850511
    ## [5,] -0.9669288 0.5589147 -0.5097552
    ## [6,] -0.7137077 0.8314972 -0.3756491

``` r
# Add the information about the different groups back into PCA data
pca_data <- data.frame(pca$x, Grammy = albums$Grammy)
head(pca_data)
```

    ##          PC1       PC2        PC3    Grammy
    ## 1 -0.4935386 0.9003782 -1.0291212 nominated
    ## 2 -0.9983281 0.8508497 -0.5766648        no
    ## 3 -0.7487569 0.9044067 -0.5464634 nominated
    ## 4 -0.7630218 0.7981912 -0.5850511        no
    ## 5 -0.9669288 0.5589147 -0.5097552        no
    ## 6 -0.7137077 0.8314972 -0.3756491        no

``` r
# Plot the data according to the new coordinate system: PC1 and PC2
ggplot(pca_data, aes(x = PC1, y = PC2, color = Grammy)) + 
  geom_point()
```

<img src="project-1-bioinfo_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
# Take a look at the rotation matrix
pca$rotation
```

    ##                                  PC1        PC2        PC3
    ## Metacritic_Score          -0.6467770  0.3356639 -0.6848425
    ## Billboard_Peak_Position   -0.3634954 -0.9250654 -0.1101138
    ## Weeks_on_Billboard_Charts  0.6704853 -0.1777180 -0.7203234

``` r
# Save the rotation matrix in a data frame
rotation_data <- data.frame(
  pca$rotation, 
  variable = row.names(pca$rotation))
# Define an arrow style
arrow_style <- arrow(length = unit(0.05, "inches"), type = "closed")
# Plot the contribution of variables to PCs using geom_segment() for arrows and geom_text() for labels
ggplot(rotation_data) + 
  geom_segment(aes(xend = PC1, yend = PC2), x = 0, y = 0, arrow = arrow_style) + 
  geom_text(aes(x = PC1, y = PC2, label = variable), hjust = 0, size = 3, color = "red") + 
  xlim(-1., 1.25) + 
  ylim(-1., 1.) +
  coord_fixed()
```

<img src="project-1-bioinfo_files/figure-gfm/unnamed-chunk-7-2.png" style="display: block; margin: auto;" />

``` r
# Determine the percentage of variance explained by each component with sdev
percent <- 100* (pca$sdev^2 / sum(pca$sdev^2))
percent
```

    ## [1] 53.95078 30.40053 15.64869

``` r
# Visualize the percentage of variance explained by each component
perc_data <- data.frame(percent = percent, PC = 1:length(percent))
ggplot(perc_data, aes(x = PC, y = percent)) + 
  geom_col() + 
  geom_text(aes(label = round(percent, 2)), size = 4, vjust = -0.5) + 
  ylim(0, 80)
```

<img src="project-1-bioinfo_files/figure-gfm/unnamed-chunk-7-3.png" style="display: block; margin: auto;" />
\#\# Conlusion From the results of the PCA and plots, I found a
correlation between metacritic score and weeks on the billboard charts.
I also saw the differences of thd means between the weeks on the
billboard of grammy winners and people not recognized, being 141 and 5
respectivley.I expected that there would be a correlation between grammy
wins and if billboard peak position. With the results the biggest factor
was the weeks on the billboard charts which played a role in the
metacritic score and if the album won a grammy.

## References

1.Metacritic Data
<https://www.metacritic.com/feature/best-albums-of-the-decade-2010s> 2.
Grammy Data
<https://en.wikipedia.org/wiki/Grammy_Award_for_Album_of_the_Year>
3.Billboard Data <https://www.billboard.com/>
