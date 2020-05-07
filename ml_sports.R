library("scatterplot3d",lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
team_cluster <- read_csv("~/src/aibook/src/chapter7/data/nba_2017_att_val_elo_win_housing_cluster.csv",col_types = cols(X1 = col_skip()))

cluster_to_numeric <- function(column){
   converted_column <- as.numeric(unlist(column))
   return(converted_column)
   }

team_cluster$pcolor[team_cluster$cluster == 0] <- "red"
team_cluster$pcolor[team_cluster$cluster == 1] <- "blue"
team_cluster$pcolor[team_cluster$cluster == 2] <- "darkgreen"

s3d <- scatterplot3d(
  cluster_to_numeric(team_cluster["VALUE_MILLIONS"]),
  cluster_to_numeric(
    team_cluster["MEDIAN_HOME_PRICE_COUNTY_MILLIONS"]),
  cluster_to_numeric(team_cluster["ELO"]),
  color = team_cluster$pcolor,
  pch=19,
  type="h",
  lty.hplot=2,
  main="3-D Scatterplot NBA Teams 2016-2017:
Value, Performance, Home Prices with kNN Clustering",
  zlab="Team Performance (ELO)",
  xlab="Value of Team in Millions",
  ylab="Median Home Price County Millions"
  )

s3d.coords <- s3d$xyz.convert(
  cluster_to_numeric(team_cluster["VALUE_MILLIONS"]),
  cluster_to_numeric(
    team_cluster["MEDIAN_HOME_PRICE_COUNTY_MILLIONS"]),
  cluster_to_numeric(team_cluster["ELO"]))
#plot text
text(s3d.coords$x, s3d.coords$y, # x and y coordinates
     labels=team_cluster$TEAM, # text to plot
     pos=4, cex=.6) # shrink text)

team_cluster <- read_csv("nba_cluster.csv",col_types = cols(X1 = col_skip()))
library("ggplot2")

#Name Clusters
team_cluster$cluster_name[team_cluster$cluster == 0] <- "Low"
Unknown or uninitialised column: 'cluster_name'.
team_cluster$cluster_name[team_cluster$
                              cluster == 1] <- "Medium Valuation/High Performance"
team_cluster$cluster_name[team_cluster$
                              cluster == 2] <- "High Valuation/Low Performance"



p <- ggplot(data = team_cluster) 
   geom_point(mapping = aes(x = ELO,
                              y = VALUE_MILLIONS,
                              color =
                               factor(WINNING_SEASON, labels=
                                        c("LOSING","WINNING")),
                             size = MEDIAN_HOME_PRICE_COUNTY_MILLIONS,
                              shape = CONF)) 
   facet_wrap(~ cluster_name) 
   ggtitle("NBA Teams 2016-2017 Faceted Plot") 
   ylab("Value NBA Team in Millions") 
   xlab("Relative Team Performance (ELO)") 
   geom_text(aes(x = ELO, y = VALUE_MILLIONS,
                   label=ifelse(VALUE_MILLIONS>1200,
                                  as.character(TEAM),'')),hjust=.35,vjust=1)
   
   
#Change legends
guides(color = guide_legend(title = "Winning Season")) 
guides(size = guide_legend(title = "Median Home Price County in Millions" )) 
guides(shape = guide_legend(title = "NBA Conference"))
   
   
player_cluster <- read_csv("nba_2017_players_social_with_clusters.csv",col_types = cols(X1 = col_skip()))
library("ggplot2")   

#Name Clusters
   player_cluster$cluster_name[player_cluster$
                                 cluster == 0] <- "Low Pay/Low"
player_cluster$cluster_name[player_cluster$
                                 cluster == 1] <- "High Pay/Above Average Performance"
player_cluster$cluster_name[player_cluster$
                                 cluster == 2] <- "Low Pay/Average Performance"
player_cluster$cluster_name[player_cluster$
                                 cluster == 3] <- "High Pay/High Performance"
player_cluster$cluster_name[player_cluster$
                                 cluster == 4] <- "Medium Pay/Above Average Performance"


#Create faceted plot
  p <- ggplot(data = player_cluster) 
  geom_point(mapping = aes(x = WINS_RPM,y = POINTS,color = SALARY_MILLIONS,size = PAGEVIEWS))facet_wrap(~ cluster_name)
  ggtitle("NBA Players Faceted") 
  ylab("POINTS PER GAME") 
  xlab("WINS ATTRIBUTABLE TO PLAYER (WINS_RPM)") 
  geom_text(aes(x = WINS_RPM, y = POINTS,

                label=ifelse(
                   PAGEVIEWS>10000|TOV>5|AGE>37|WINS_RPM>15|cluster == 2 & WINS_RPM > 3,as.character(PLAYER),'')),hjust=.8, check_overlap = FALSE)
#Change legends
guides(color = guide_legend(title = "Salary Millions")) +
guides(size = guide_legend(title = "Wikipedia Daily Pageviews" ))+scale_color_gradientn(colours = rainbow(3))
geom_text(aes(x = ELO, y = VALUE_MILLIONS, label=ifelse(VALUE_MILLIONS>1200,as.character(TEAM),'')),hjust=.35,vjust=1)