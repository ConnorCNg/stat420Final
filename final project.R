raw_data = read.csv("./fifa18-all-player-statistics-master/2019/data.csv")

library(dplyr)
#remove obviously unuseful variables
file = select(raw_data,-c(X,ID, Photo, Flag, Club.Logo, Loaned.From))

file$LS = as.character(file$LS)
file = subset(file, file$LS != "")
temp = as.vector(strsplit(file$LS, split = "+"))
original = rep(0, length(temp))
after = rep(0, length(temp))
for(i in 1 : length(temp)){
  str = temp[[i]]
  original[i] = as.numeric(paste(str[1], str[2], sep = ""))
  after[i] = original[i] + as.numeric(str[4])
}

#convert columns LS to RB to numeric values
# take the values with plus sign and without plus sign
position_raw = file[, 23:48]
original = matrix(0, nrow(position_raw), ncol(position_raw))
after = matrix(0, nrow(position_raw), ncol(position_raw)) #######THIS IS LS TO RB WITH PLUS SIGN!!!
for(i in 1:nrow(position_raw)){
  for(j in 1:ncol(position_raw)){
    str = as.character(position_raw[i, j])
    if(str == ""){
      original[i, j] = NA
      after[i, j] = NA
    } else{
      str = strsplit(str, split = "+")[[1]]
      original[i,j] = as.numeric(paste(str[1], str[2], sep = ""))
      after[i,j] = original[i] + as.numeric(str[4])
    }
  }
}

file[, 23:48] = original
file = na.omit(file)

#convert Value into numeric
file$Value = as.character(file$Value)
#remove euro sign and million
temp = substring(strsplit(file$Value, split = "M"), 2) ##########NOTE: this value is in Million
file$Value = as.numeric(temp)
#remove NA
anyNA(file$Value)
if(anyNA(file$Value)){
  file = na.omit(file)
}

#convert wage into numeric
file$Wage = as.character(file$Wage)
#remove euto sign and K 
temp = substring(strsplit(file$Wage, split = "K"), 2) ###########NOTE: this value is in K
file$Wage = as.numeric(temp)
#remove NA
anyNA(file$Wage)


#convert release clause into numeric
file$Release.Clause = as.character(file$Release.Clause)
#remove euro sign and M
temp = substring(strsplit(file$Release.Clause, split = "M"), 2) #########NOTE: this value is in M
file$Release.Clause = as.numeric(temp)
#remove NA
if(anyNA(file$Release.Clause)){
  file = na.omit(file)
}

#convert weight from factor to numeric
file = subset(file, file$Weight != "")
file$Weight = substr(file$Weight, 1, 3)

#convert height to numeric
file = subset(file, file$Height != "")
file$Height = as.numeric(gsub("\'", ".", file$Height)) * 0.3048 ########Note: in meters

