# IRONMAN data

library(dplyr)
library(XML)
library(ggplot2)

#url = "http://www.ironman.com/triathlon/events/americas/ironman/world-championship/results.aspx?p=1&race=worldchampionship&rd=20141011&agegroup=Pro&sex=M&ps=20#axzz3IO9hXung"
#tables = readHTMLTable(url)
#df = tables[[1]]
#df = df[0, ]

df = data.frame()

year = c("2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003")
urlparam = c("20141011", "20131012", "20121013", "20111008", "20101009", "20091010", "20081011", "20071013", 
             "20061021", "20051015", "20041016", "20031018")
datedf = data.frame(year, urlparam)

# need to do 2002 separately

#nrow(datedf)

for (g in 1:2){
  gender = "M"
  if (g==2){
    gender = "F"
  }
  
  for (y in 1:nrow(datedf)){
    for (i in 1:1){
      
      url = paste0("http://www.ironman.com/handlers/eventresults.aspx?p=", i,"&race=worldchampionship&rd=",
                   datedf[y, "urlparam"], "&agegroup=Pro&sex=", gender, "&ps=20&so=drank")

      tables = readHTMLTable(url)

      print(tables)
      
      if (length(tables) == 0){
        next
      }
      tempdf = tables[[1]]
      tempdf$year = datedf[y, "year"]
      tempdf$gender = gender
      df = rbind(df, tempdf)
    }
  }
}

df[ df == "---" ] = NA

names(df) = c("Name", "Country", "DivRank", "GenderRank", "OverallRank", 
              "Swim", "Bike", "Run", "Finish", "Points", "Year", "Gender")

df$DivRank = as.numeric(as.character(df$DivRank))
df$GenderRank = as.numeric(as.character(df$GenderRank))
df$OverallRank = as.numeric(as.character(df$OverallRank))

df$Swim = as.character(df$Swim)
df$Bike = as.character(df$Bike)
df$Run = as.character(df$Run)

substr(df$Swim, 1, 2)

df = mutate(df,
       Swim_hr = as.numeric(substr(Swim, 1, 2)),
       Swim_min = as.numeric(substr(Swim, 4, 5)),
       Swim_sec = as.numeric(substr(Swim, 7, 8)),
       Swim_total_min = (Swim_hr*60) + Swim_min + (Swim_sec/60))

df = mutate(df,
       Bike_hr = as.numeric(substr(Bike, 1, 2)),
       Bike_min = as.numeric(substr(Bike, 4, 5)),
       Bike_sec = as.numeric(substr(Bike, 7, 8)),
       Bike_total_min = (Bike_hr*60) + Bike_min + (Bike_sec/60))

df = mutate(df,
       Run_hr = as.numeric(substr(Run, 1, 2)),
       Run_min = as.numeric(substr(Run, 4, 5)),
       Run_sec = as.numeric(substr(Run, 7, 8)),
       Run_total_min = (Run_hr*60) + Run_min + (Run_sec/60))

df = mutate(df,
       Finish_hr = as.numeric(substr(Finish, 1, 2)),
       Finish_min = as.numeric(substr(Finish, 4, 5)),
       Finish_sec = as.numeric(substr(Finish, 7, 8)),
       Finish_total_min = (Finish_hr*60) + Finish_min + (Finish_sec/60))

fastest = select(df, Name, Gender, Country, Finish, Finish_total_min, Year)
fastest = filter(fastest, !is.na(Finish_total_min))
fastest = arrange(fastest, Finish_total_min)

head(fastest)

head(filter(fastest, Gender=="F"))



# ========= Graphing =========== #


p = ggplot(df, aes(Bike_total_min, Run_total_min))
p + geom_point(aes(colour = factor(df$Year), size = 5)) +
    scale_x_continuous(limits = c(250, 400)) +
    scale_y_continuous(limits = c(150, 300))




