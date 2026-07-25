#install.packages('DT')
library('DT')
library('ggplot2')
library('shiny')
library('bslib')
library('tidyr')
library('dplyr')
library('ggplot2')
library('htmlwidgets')
library('reshape2')
library('ggtext')
library('googlesheets4')
library('lubridate')
library('formattable')
library(DBI)
library(RPostgres)


# Functions:
makeStatTable<-function(stat_data){
  if(nrow(stat_data)==0){
    empty_table<-data.frame(c("No data to show - Please try other combinations."))
    colnames(empty_table)<-"Empty table!"
    return(empty_table)
  } else {
    tmp_player_list<-unique(stat_data$ID)
    tmp_stats_table<-data.frame(ID=tmp_player_list)
    tmp_stats_table$gp<-NA
    tmp_stats_table$gw<-NA
    tmp_stats_table$ps<-NA
    tmp_stats_table$adj_ps<-NA
    tmp_stats_table$pp<-NA
    tmp_stats_table$gw_div_gp<-NA
    tmp_stats_table$ps_div_pp<-NA
    tmp_stats_table$adj_ps_div_pp<-NA
    tmp_stats_table$beta<-NA # New stat, to be calculated with adj_ps_div_pp*(1+((gw_div_gp-0.5)/2)) ... this centres gw/gp around zero, with min/max -0.25,0.25 (would be -0.5,0.5, but divided by 2)
    tmp_stats_table$rank_4dr<-NA
    tmp_stats_table$sp<-NA
    tmp_stats_table$sa<-NA
    tmp_stats_table$sp_div_sa<-NA
    # Look for calcs:
    for (i in tmp_player_list){
      tmp_stats_table[tmp_stats_table$ID==i,]$gp<-
        length(stat_data[stat_data$ID==i,1])
      tmp_stats_table[tmp_stats_table$ID==i,]$gw<-
        length(stat_data[stat_data$ID==i &
                           stat_data$score_side>stat_data$score_opp,1])
      tmp_stats_table[tmp_stats_table$ID==i,]$ps<-
        sum(stat_data[stat_data$ID==i,]$score_side)
      tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps<-
        round(sum(stat_data[stat_data$ID==i,]$score_side_adj),2) # Rounded to 2dp ... probably should be un-rounded
      tmp_stats_table[tmp_stats_table$ID==i,]$pp<-
        sum(stat_data[stat_data$ID==i,]$score_side)+
        sum(stat_data[stat_data$ID==i,]$score_opp)
      tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp<-
        round(tmp_stats_table[tmp_stats_table$ID==i,]$gw/
                tmp_stats_table[tmp_stats_table$ID==i,]$gp,3)
      tmp_stats_table[tmp_stats_table$ID==i,]$ps_div_pp<-
        round(tmp_stats_table[tmp_stats_table$ID==i,]$ps/
                tmp_stats_table[tmp_stats_table$ID==i,]$pp,3)
      tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp<-
        round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps/
                tmp_stats_table[tmp_stats_table$ID==i,]$pp,3) # Rounded to 3dp ... (see above)
      tmp_stats_table[tmp_stats_table$ID==i,]$beta<-
        round(tmp_stats_table[tmp_stats_table$ID==i,]$adj_ps_div_pp *
                (1+((tmp_stats_table[tmp_stats_table$ID==i,]$gw_div_gp-0.5)/2)),3) # New stat (see rationale above)
      tmp_stats_table[tmp_stats_table$ID==i,]$rank_4dr<-
        round(rank_table[rank_table$ID==i,]$rank,4)
      #NB the following assumes a maximum of ONE 'session' per DATE.
      tmp_stats_table[tmp_stats_table$ID==i,]$sp<-
        length(unique(as_date(stat_data[stat_data$ID==i,]$date_time)))
      tmp_stats_table[tmp_stats_table$ID==i,]$sa<-
        length(unique(as_date(stat_data$date_time)))
      tmp_stats_table[tmp_stats_table$ID==i,]$sp_div_sa<-
        round(tmp_stats_table[tmp_stats_table$ID==i,]$sp/
                tmp_stats_table[tmp_stats_table$ID==i,]$sa*100,0)
      
    }
    return(tmp_stats_table)
  }
}
makePlot<-function(plot_data,plot_label){
  ## select data
  stats_select<-plot_data %>% select(ID,gw_div_gp,ps_div_pp,adj_ps_div_pp,beta)
  # sort by ranking:
  stats_select<- stats_select %>% arrange(desc(beta))
  plot_data<- plot_data %>% arrange(desc(beta)) # required for attendance labels
  col<-case_when(plot_data$sp_div_sa<25 ~ 'red',
                 plot_data$sp_div_sa<50 & plot_data$sp_div_sa>24 ~ 'darkred',
                 plot_data$sp_div_sa>49 ~ 'black')
  # change to long format
  stats_select_long <- melt(stats_select, id.vars="ID")
  stats_select_long$ordering <- 1:length(stats_select_long$ID)
  
  g<-ggplot(stats_select_long, aes(x=reorder(ID,ordering),y=value,fill=variable)) +
    geom_col(position='dodge')+
    geom_text(aes(y=1,label = ifelse(variable == "beta",
                                     paste0("\n",plot_data$sp,"/",
                                            plot_data$sa,"\n",
                                            plot_data$sp_div_sa,"%"),"")),
              size=2)+
    ylim(0,1)+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_classic() +
    ggtitle(plot_label)+
    theme(axis.text.x = element_text(size = 12))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(legend.position = "bottom", legend.box = "horizontal", legend.title=element_blank()) +
    theme(axis.text.x = element_markdown(colour=col))+ # element_markdown avoids array for colour causing error (don't know why!)
    scale_fill_manual(values=c("purple","orange","grey","black"), labels = c("games won / games played", 
                                                                             "points won / points played",
                                                                             "adj. points won / points played",
                                                                             "beta"))
  return(g)
}
makePlotVert<-function(plot_data,plot_label){
  ## select data
  stats_select<-plot_data %>% select(ID,gw_div_gp,ps_div_pp,adj_ps_div_pp,beta)
  # sort by ranking:
  stats_select<- stats_select %>% arrange(beta)
  plot_data<- plot_data %>% arrange(beta) # required for attendance labels
  col<-case_when(plot_data$sp_div_sa==50 ~ 'darkgrey',
                 plot_data$sp_div_sa<50 ~ 'red',
                 plot_data$sp_div_sa>50 ~ 'black')
  # change to long format
  stats_select_long <- melt(stats_select, id.vars="ID")
  stats_select_long$ordering <- 1:length(stats_select_long$ID)
  
  g<-ggplot(stats_select_long, aes(x=reorder(ID,ordering),y=value,fill=variable)) +
    geom_col(position='dodge')+
    geom_text(aes(y=0.9,vjust = -0.01,label = ifelse(variable == "beta",
                                                     paste0("\n",plot_data$sp,"/",
                                                            plot_data$sa,"; ",
                                                            plot_data$sp_div_sa,"%"),"")),
              size=2)+
    ylim(0,1)+
    #scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_classic() +
    ggtitle(plot_label)+
    theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 10))+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    theme(axis.text.y = element_markdown(colour=col))+ # element_markdown avoids array for colour causing error (don't know why!)
    scale_fill_manual(values=c("purple","orange","grey","black"), labels = c("games won / games played", 
                                                                             "points won / points played",
                                                                             "adj. points won / points played",
                                                                             "beta"))+
    coord_flip()+
    theme(legend.position = "bottom", legend.direction = "vertical", legend.title=element_blank())
  return(g)
}
makePlot4dr<-function(plot_data){
  # order:
  stats_select<-plot_data %>% select(ID,rank_4dr,gp)
  stats_select<- stats_select %>% arrange(desc(rank_4dr))
  stats_select$ordering <- 1:length(stats_select$ID)
  
  g <- ggplot(stats_select, aes(x=reorder(ID,ordering),y=rank_4dr)) +
    geom_col()+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    geom_text(aes(label = paste0(format(round(rank_4dr,4), nsmall=4))), #," (",round(gp,0)," games)")),
              hjust = 1, vjust=0.5,
              position = position_nudge(x =0, y=-0.2), color = "green", angle=90, size = 3) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11))+
    ggtitle('4DR ranking')+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  return(g)
}
makePlot4drVert<-function(plot_data){
  # order:
  stats_select<-plot_data %>% select(ID,rank_4dr,gp)
  stats_select<- stats_select %>% arrange(rank_4dr)
  stats_select$ordering <- 1:length(stats_select$ID)
  
  g <- ggplot(stats_select, aes(x=reorder(ID,ordering),y=rank_4dr)) +
    geom_col()+
    geom_text(aes(label = paste0(format(round(rank_4dr,4), nsmall=4))), #," (",round(gp,0)," games)")),
              hjust = 1, vjust=0.4, 
              position = position_nudge(x =0, y=-0.2), color = "green", size = 2.5) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 9),axis.text.y = element_text(size = 9))+
    ggtitle('4DR ranking')+
    theme(axis.title.x=element_blank(),axis.title.y=element_blank())+
    coord_flip()
  return(g)
}
sequential_ranks_calc<-function(ID){
  id_count=1
  data_output<-sequential_ranks[0,] # Create empty table with same columns
  print(sequential_ranks[1:5,])
  print(summary(sequential_ranks))
  while(id_count<=length(ID)){
    #dates<-unique(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)
    dates<-unique(as_date(sequential_ranks[sequential_ranks$ID==ID[id_count],]$date_time)) # Isolates unique dates from date-times for each participant (ID)
    for(i in dates){
      one_row<-sequential_ranks[sequential_ranks$ID==ID[id_count],]
      one_row<-one_row[one_row$date_time==
                         max(one_row$date_time[one_row$date_time<(as_date(i)+dhours(24))]),]
      one_row<- one_row %>% arrange(ymd_hms(date_time)) %>% slice(n())
      data_output<-data_output %>% add_row(one_row)
    }
    id_count<-id_count+1
  }
  return(data_output)
}
processInputs<-function(indoor,location,day,date,eventType){
  if(indoor=="Indoor"){
    choice_indoor=1
  } else if(indoor=="Outdoor"){
    choice_indoor=0
  } else if(indoor=="Both"){
    choice_indoor=c(0,1)
  }
  # Read-in Location selection
  if(location=="All"){
    choice_location=unique(as.character(match_table$location))
  } else {
    choice_location=location
  }
  # Read-in Day selection
  if(day=="All"){
    choice_day=unique(as.character(match_table$dow))
  } else if(day=="Weekend"){
    choice_day=c("Fri","Sun")
  } else {
    choice_day=day
  }
  # Read-in Date selection
  if(date=="All"){
    choice_date<-unique(match_table$date_time)
  } else {
    choice_date<-match_table$date_time[match_table$date_time>as_date(date) & 
                                         match_table$date_time<as_date(date)+dhours(24)]
  }
  # Read-in Event selection
  if(eventType=="All"){
    choice_event_type=unique(match_table$event_type)
  } else {
    choice_event_type=eventType
  }
  
  input_values <- list("choice_indoor" = choice_indoor,
                       "choice_location" = choice_location,
                       "choice_day" = choice_day,
                       "choice_date" = choice_date,
                       "choice_event_type" = choice_event_type)
  return(input_values) 
}
fourDRCalc_zeroSum_ORIGINAL<-function(){
  for (i in 1:game_max){
    tmp_rank_table<-rank_table # after each game, current ranks stored in tmp_table
    
    # Create table for single game, i
    game_table<-match_table_long[match_table_long$game==i,]
    
    # Store four players' starting ranks:
    player_one_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[1],2]
    player_two_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[2],2]
    player_three_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[3],2]
    player_four_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[4],2]
    
    for (row_num in 1:4){
      ## Assign pre-game ranks to players for each row (player):
      if (row_num == 1) {
        own_rank<-player_one_rank
        partner_rank<-player_two_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 2) {
        own_rank<-player_two_rank
        partner_rank<-player_one_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 3) {
        own_rank<-player_three_rank
        partner_rank<-player_four_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      } else {
        own_rank<-player_four_rank
        partner_rank<-player_three_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      }
      
      scalar_adj<-1 # Number to divide side ranks by to balance probability of a win
      
      if (game_table$score_side[row_num]>game_table$score_opp[row_num]) { # i.e. player won
        prob<-exp((opponent_1_rank+opponent_2_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_opp[row_num]/
                        game_table$score_side[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(0.1*prob)-(0.001*points_diff)
        ## Calculate new rank
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        if(new_rank < 1) new_rank<-1
        ## Assign new rank to rank_table
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
        #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
        #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        
        ## Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
        
      } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
        prob<-exp((own_rank+partner_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_side[row_num]/
                        game_table$score_opp[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(-0.1*prob)+(0.001*points_diff)
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        if(new_rank < 1) new_rank<-1
        rank_table[rank_table$ID==game_table$ID[row_num],2]<<-new_rank
        #rank_table[rank_table$ID==game_table$ID[row_num],2]<<-
        #  tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                        rank4dr = rank_table[rank_table$ID==game_table$ID[row_num],2],
                                                        date=game_table$date[row_num])
      } else { print("No-difference in score")} # therefore 4dr rank not updated
    }
  }
}
fourDRCalc_zeroSum<-function(rank_table,game_max,match_table_long){
  # Create copy of rank table for function:
  fx_rank_table<-rank_table
  # Create table to collect sequential 4DR ranks:
  sequential_ranks<-data.frame(ID=character(),rank4dr=numeric(),date_time=ymd_hms(),
                               stringsAsFactors=FALSE)
  for (i in 1:game_max){
    # Create tmp copy of rank table for function:
    tmp_rank_table<-fx_rank_table
    
    # Create table for single game, i
    game_table<-match_table_long[match_table_long$game==i,]
    
    # Store four players' starting ranks:
    player_one_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[1],2]
    player_two_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[2],2]
    player_three_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[3],2]
    player_four_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[4],2]
    
    for (row_num in 1:4){
      ## Assign pre-game ranks to players for each row (player):
      if (row_num == 1) {
        own_rank<-player_one_rank
        partner_rank<-player_two_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 2) {
        own_rank<-player_two_rank
        partner_rank<-player_one_rank
        opponent_1_rank<-player_three_rank
        opponent_2_rank<-player_four_rank
      } else if (row_num == 3) {
        own_rank<-player_three_rank
        partner_rank<-player_four_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      } else {
        own_rank<-player_four_rank
        partner_rank<-player_three_rank
        opponent_1_rank<-player_one_rank
        opponent_2_rank<-player_two_rank
      }
      
      scalar_adj<-1 # Number to divide side ranks by to balance probability of a win
      
      if (game_table$score_side[row_num]>game_table$score_opp[row_num]) { # i.e. player won
        prob<-exp((opponent_1_rank+opponent_2_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_opp[row_num]/
                        game_table$score_side[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(0.1*prob)-(0.001*points_diff)
        ## Calculate new rank
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        if(new_rank < 1) new_rank<-1
        ## Assign new rank to rank_table
        fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2]<-new_rank
        
        ## Add ID, rank and date to sequential ranks table:
        sequential_ranks<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                       rank4dr = fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2],
                                                       date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
        
      } else if (game_table$score_side[row_num]<game_table$score_opp[row_num]) { # i.e. player lost
        prob<-exp((own_rank+partner_rank)/scalar_adj)/
          (exp((own_rank+partner_rank)/scalar_adj)+exp((opponent_1_rank+opponent_2_rank)/scalar_adj))
        points_diff<-(game_table$score_side[row_num]/
                        game_table$score_opp[row_num])*11 #alternative points diff, normalised to 11-pt game [8/5/25]
        points_awarded<-(-0.1*prob)+(0.001*points_diff)
        ## Set floor of 1.000 for ranks [Update 25022026)]:
        new_rank<-tmp_rank_table[tmp_rank_table$ID==game_table$ID[row_num],2] + points_awarded
        if(new_rank < 1) new_rank<-1
        fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2]<-new_rank
        
        # Add ID, rank and date to sequential ranks table:
        sequential_ranks<-sequential_ranks %>% add_row(ID = game_table$ID[row_num],
                                                       rank4dr = fx_rank_table[fx_rank_table$ID==game_table$ID[row_num],2],
                                                       date_time=ymd_hms(game_table$date_time[row_num])) # added ymd_hms 20032026
      } else { print("No-difference in score")} # therefore 4dr rank not updated
    }
  }
  returns <- list(ranks=fx_rank_table,seqRanks=sequential_ranks)
  return(returns)
}
createLeaderBoard<-function(data_instance,data_instance_pen,row_length){
  #Select current rows that match >=50% attendance criteria and create ranks
  current_ladder_table<-data_instance[data_instance$sp_div_sa>=50,] %>%
    mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
    mutate(rank=rank(desc(rating),ties.method = 'max'))
  
  #Check 'row_length' for '0' (i.e. all rows) and that it doesn't exceed no. of rows:
  if (row_length > nrow(current_ladder_table)){
    row_length <- nrow(current_ladder_table)
  } else if (row_length == 0) {
    row_length <- nrow(current_ladder_table)
  }
  
  #Create table of penultimate session only if data available
  if (is.data.frame(data_instance_pen)){ # If the penultimate stats don't exist, this function is passed '0' by server code, i.e. not a data.frame
    penultimate_ladder_table<-data_instance_pen %>%
      mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
      filter(ID %in% current_ladder_table$ID) %>%
      mutate(rank=rank(desc(rating),ties.method = 'max'))
    
    current_ladder_table$change=NA
    for (i in current_ladder_table$ID[current_ladder_table$ID %in% 
                                      penultimate_ladder_table$ID]){
      current_ladder_table[current_ladder_table$ID == i,]$change <-
        penultimate_ladder_table[penultimate_ladder_table$ID == i,]$rank -
        current_ladder_table[current_ladder_table$ID == i,]$rank
    }
    
    # Replace 'NAs', i.e. missing from penultimate ladder, with 'new' to indicate new players
    current_ladder_table$new<-""
    if(length(current_ladder_table[is.na(current_ladder_table$change),]$change)>0){
      current_ladder_table[is.na(current_ladder_table$change),]$new<-"new"
    }
  } else {
    current_ladder_table$change<-NA
    current_ladder_table$new<-"new"
  }
  
  # Generate RANK for current (all data) and preceding week (all data minus current)
  # Identify new-entrants
  
  # Custom formatted:
  improvement_formatter <- 
    formatter("span", 
              style = x ~ style(
                font.weight = ifelse(x > 0, "bold", ifelse(x < 0, "italic", "normal")),
                color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))),
              x ~ icontext(ifelse(x>0, "arrow-up", ifelse(x<0,"arrow-down","blank"))))
  
  # Formattable table:
  f<-formattable(current_ladder_table[1:row_length,],
                 align=c("l","c","c","c","c"),
                 col.names = c("","rating","rank", "Δ", ""),
                 list(
                   `rating` = color_tile("transparent","violet"),
                   `change` = improvement_formatter),
                 table.attr = 'style="font-size: 11px;";\"')
}
createLeaderBoard_4dr<-function(data_instance,row_length){
  #Select current rows that match >=50% attendance criteria and create ranks
  current_ladder_table<-data_instance[data_instance$sp_div_sa>=25,] %>%
    mutate(rating=rank_4dr) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
    mutate(rank=rank(desc(rating),ties.method = 'max'))
  
  #Check 'row_length' for '0' (i.e. all rows) and that it doesn't exceed no. of rows:
  if (row_length > nrow(current_ladder_table)){
    row_length <- nrow(current_ladder_table)
  } else if (row_length == 0) {
    row_length <- nrow(current_ladder_table)
  }
  
  # #Create table of penultimate session only if data available
  # if (is.data.frame(data_instance_pen)){ # If the penultimate stats don't exist, this function is passed '0' by server code, i.e. not a data.frame
  #   penultimate_ladder_table<-data_instance_pen %>%
  #     mutate(rating=beta) %>% select(ID,rating) %>% arrange(desc(rating)) %>%
  #     filter(ID %in% current_ladder_table$ID) %>%
  #     mutate(rank=rank(desc(rating),ties.method = 'max'))
  #   
  #   current_ladder_table$change=NA
  #   for (i in current_ladder_table$ID[current_ladder_table$ID %in% 
  #                                     penultimate_ladder_table$ID]){
  #     current_ladder_table[current_ladder_table$ID == i,]$change <-
  #       penultimate_ladder_table[penultimate_ladder_table$ID == i,]$rank -
  #       current_ladder_table[current_ladder_table$ID == i,]$rank
  #   }
  #   
  #   # Replace 'NAs', i.e. missing from penultimate ladder, with 'new' to indicate new players
  #   current_ladder_table$new<-""
  #   if(length(current_ladder_table[is.na(current_ladder_table$change),]$change)>0){
  #     current_ladder_table[is.na(current_ladder_table$change),]$new<-"new"
  #   }
  # } else {
  #   current_ladder_table$change<-NA
  #   current_ladder_table$new<-"new"
  # }
  
  # Generate RANK for current (all data) and preceding week (all data minus current)
  # Identify new-entrants
  
  # Custom formatted:
  improvement_formatter <- 
    formatter("span", 
              style = x ~ style(
                font.weight = ifelse(x > 0, "bold", ifelse(x < 0, "italic", "normal")),
                color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))),
              x ~ icontext(ifelse(x>0, "arrow-up", ifelse(x<0,"arrow-down","blank"))))
  
  # Formattable table:
  f<-formattable(current_ladder_table[1:row_length,],
                 align=c("l","c","c"), #"c","c"),
                 col.names = c("","rating","rank"), #"Δ", ""),
                 list(
                   `rating` = color_tile("transparent","violet")),
                 #`change` = improvement_formatter),
                 table.attr = 'style="font-size: 11px;";\"')
}
# DB functions:
connectDB <- function(){
  
  # Set Supabase connection env. parameters:
  Sys.setenv(SUPABASE_DB_HOST = "aws-1-eu-west-1.pooler.supabase.com")
  Sys.setenv(SUPABASE_DB_PORT = "5432")
  Sys.setenv(SUPABASE_DB_NAME = "postgres")
  Sys.setenv(SUPABASE_DB_USER = "postgres.bnnisnnqvsghpyktijal")
  
  Sys.setenv(SUPABASE_DB_PASS = Sys.getenv("SUPABASE_PW"))   # Password stored on Connect Cloud: Admin/Settings > Variables
  
  # Store environment parameters in variables:
  host <- Sys.getenv("SUPABASE_DB_HOST", "db.bnnisnnqvsghpyktijal.supabase.co") # Not sure if second argument is needed? Same for next few lines
  port <- as.integer(Sys.getenv("SUPABASE_DB_PORT", "5432"))
  dbname <- Sys.getenv("SUPABASE_DB_NAME", "postgres")
  user <- Sys.getenv("SUPABASE_DB_USER", "postgres")
  password <- Sys.getenv("SUPABASE_DB_PASS")
  if (!nzchar(password)) stop("Set SUPABASE_DB_PASS environment variable")
  
  # Establish DB connection:
  con <- dbConnect(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    sslmode = "require"
  )
  return(con)
}
db_upsert<-function(db_target_table,df,columns,conflicts){
  cols <- columns # columns to upsert
  col_list <- paste(sprintf('"%s"', cols), collapse = ", ")
  val_list <- paste(sprintf("$%d", seq_along(cols)), collapse = ", ")
  
  # Conflict Key column(s) (must have a UNIQUE/PK constraint)
  conflict_cols <- conflicts # e.g. c("date_time","location","court_rank","p1","p2",...)
  conflict_target <- paste(sprintf('"%s"', conflict_cols), collapse = ", ")
  
  # Update all columns except the conflict key columns
  update_cols <- setdiff(cols, conflict_cols)
  set_clause <- paste(
    sprintf('"%s" = EXCLUDED."%s"', update_cols, update_cols),
    collapse = ", "
  )
  
  sql <- sprintf(
    'INSERT INTO public."%s" (%s) VALUES (%s)
   ON CONFLICT (%s) DO UPDATE SET %s;',
    db_target_table,col_list, val_list, conflict_target, set_clause
  )
  
  # Execute (parameterized)
  for (i in seq_len(nrow(df))) {
    tmp_params<-as.list(df[i,cols]) # This lists the row values for each NAMED column - name removed in next step
    DBI::dbExecute(con, sql, params = unname(tmp_params))
  }
}
db_replace_table<-function(db_target_table,df){
  dbBegin(con)
  dbExecute(con, sprintf('TRUNCATE TABLE "%s" RESTART IDENTITY;',db_target_table))
  
  dbWriteTable(
    con,
    name = db_target_table,
    value = df,
    append = TRUE,
    row.names = FALSE
  )
  
  dbCommit(con)
}

#Call DB connection function
con<-connectDB()

updateFx<-function(){
  ## Load data from database
  match_table <- dbReadTable(con, "mastersheet")   # equivalent to SELECT * FROM "mastersheet"
  init_4dr_table<-dbReadTable(con, "4DR_initialiser")
  
  # Format data and sort-by date
  match_table$date_time <- ymd_hms(match_table$date_time) #Convert to lubridate date/time format
  match_table <- match_table %>% arrange(date_time) #Sort table by date_time
  
  # Add Days of the Week:
  match_table$dow<-as.character(wday(match_table$date_time, label=TRUE))
  
  # Find number of games(=rows) in match_table:
  game_max<-nrow(match_table)
  
  # Create empty data.frame to store results in 'long' format (i.e. one row per player per game)
  match_table_long <- data.frame(ID=character(),
                                 date_time=as.Date(character()), #update to 'date_time' 19032026
                                 dow=character(),
                                 game=integer(),
                                 partner=character(), 
                                 opp1=character(), 
                                 opp2=character(),
                                 score_side=integer(),
                                 score_opp=integer(),
                                 score_method=character(),
                                 location=character(),
                                 indoor=integer(),
                                 no_of_courts=integer(),
                                 court_rank=integer(),
                                 event_type=character(),
                                 stringsAsFactors = FALSE)
  
  
  # Reformat to long format (see above) and store in match_table_long:
  for (i in 1:game_max){
    row1<-match_table[i,] %>%
      select(ID=p1,partner=p2,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
             location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
    row1$game<-i
    
    row2<-match_table[i,] %>%
      select(ID=p2,partner=p1,opp1=p3,opp2=p4,score_side=p1p2_score,score_opp=p3p4_score,score_method,
             location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
    row2$game<-i
    
    row3<-match_table[i,] %>%
      select(ID=p3,partner=p4,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
             location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
    row3$game<-i
    
    row4<-match_table[i,] %>%
      select(ID=p4,partner=p3,opp1=p1,opp2=p2,score_side=p3p4_score,score_opp=p1p2_score,score_method,
             location,date_time=date_time,dow=dow,indoor,no_of_courts,court_rank,event_type)
    row4$game<-i
    
    match_table_long<-bind_rows(match_table_long,row1,row2,row3,row4)
  }
  
  # Store list of all players in match_table:
  player_list<-unique(match_table_long$ID) # I think this should now be pullede straight from member list??
  
  ## Rank tables
  # Create table of ranks with all players as 3.000:
  rank_table<-data.frame(ID=player_list,rank=3)
  
  # Merge with historical ranks to replace '3.000's where known ('init_4dr_table')
  for(id in 1:nrow(init_4dr_table)){
    rank_table$rank[rank_table$ID %in% init_4dr_table$name[id]] <- init_4dr_table$rank[id]
  }
  
  # Ensure ranks are numeric
  rank_table$rank<-as.numeric(rank_table$rank)
  
  ## Add 'adjusted score' column to 'match_table_long' for traditional ladders:
  # maximum (i.e. smallest!) fraction by which points are down-adjusted
  max_adj_factor<-0.8
  match_table_long$score_side_adj<-NA
  # calculate adjusted scores from court 'levels' (0=courts not assigned levels)
  for (i in 1:game_max){
    if (match_table_long[match_table_long$game==i,]$court_rank[1] == 0){
      match_table_long[match_table_long$game==i,]$score_side_adj<-
        match_table_long[match_table_long$game==i,]$score_side
    } else {
      match_table_long[match_table_long$game==i,]$score_side_adj<-
        match_table_long[match_table_long$game==i,]$score_side *
        (1-((match_table_long[match_table_long$game==i,]$court_rank-1)*
              ((1-max_adj_factor)/(match_table_long[match_table_long$game==i,]$no_of_courts-1))))
    }
  }
  
  
  ## Run 4DR calculation
  fourDR_returns<-fourDRCalc_zeroSum(rank_table,game_max,match_table_long) #updated - zero sum version; simultaneous game calcs (not sequential for the 4 players); div by 3
  rank_table<-fourDR_returns$ranks
  sequential_ranks<-fourDR_returns$seqRanks
  
  ## UPSERT ranks:
  rank_table$name<-rank_table$ID # Map ID to name for upsert - needs to match DB table
  db_upsert("4DR_current",rank_table,c("name","rank"),"name")
  
  ## REPLACE sequential ranks:
  seq_ranks_tmp<-data.frame(name=sequential_ranks$ID,
                            rank=sequential_ranks$rank4dr, # Map rank4dr to rank for upsert - needs to match DB table
                            date_time=sequential_ranks$date_time)
  db_replace_table("sequential_ranks",seq_ranks_tmp)
  
  ## REPLACE match_table_long:
  db_replace_table("match_table_long",match_table_long)
}
#updateFx()

## app.R ##
server <- function(input, output) {
  run_update <- reactive({
    updateFx()
  }) %>% bindEvent(input$update)
  
  output$time_string <- renderText({
    paste("... Update completed at: ", as.character(floor_date(ymd_hms(Sys.time()))),run_update())
  })
}

ui <- page_fluid(
  title = "CPC Stats_update",
  headerPanel(""),
  fluidRow(column(4),
           column(4,
                  actionButton(
                    "update",
                    "Re-run Db Update"),
                  p("Click once and wait 2-3 minutes for update ..."),
                  textOutput("time_string"),
                  align="center"),column(4))
  
)


shinyApp(ui = ui, server = server)