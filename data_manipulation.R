# Load Required Packages
library(stringr)

# Load in DFS Data for Draft Kings
data <- read.csv("~/dfs_project/dfs_all_weeks.csv", stringsAsFactors = FALSE)

# Create a New Player Name
for(i in 1:nrow(data)){
  data$full_name[i] <- paste(data$Name2[i], data$Name1[i])
  data$full_name[i] <- str_trim(data$full_name[i], side = "both")
}

# save salary as a numbr
data$DK.salary <- gsub("s", "", data$DK.salary)
data$DK.salary[data$DK.salary == "" | data$DK.salary == "0"] <- NA
data$DK.salary <- as.numeric(data$DK.salary)
data <- data[-which(is.na(data$DK.salary)),]

# Pull out players who have never scored any points
data <- subset(data, DK.points > 0 | Pos == "Def")

# Sample out only the week 4 or 5 players
week5 <- c(data$full_name[data$Week == 5])
data5 <- subset(data, full_name %in% week5)

# remove na functions
mean2 <- function(x){mean(x, na.rm = TRUE)}
sd2 <- function(x){sd(x, na.rm = TRUE)}
sum2 <- function(x){sum(x, na.rm = TRUE)}


# Function for sampling out one possible roster
sample_roster <- function(){
  # get the data
  dat <- data5
  
  # Create Position Subsets
  qbs <- subset(dat, Pos == "QB")
  qbs <- unique(qbs$full_name)
  rbs <- subset(dat, Pos == "RB")
  rbs <- unique(rbs$full_name)
  wrs <- subset(dat, Pos == "WR")
  wrs <- unique(wrs$full_name)
  tes <- subset(dat, Pos == "TE")
  tes <- unique(tes$full_name)
  def <- subset(dat, Pos == "Def")
  def <- unique(def$full_name)
  
  # Create the roster under constraints
  out_sal <- 1e10 
  while(out_sal > 50000 | out_sal < 45000){
    # Sample out the appropriate number of each position
    qb_no <- round(runif(1,1,NROW(qbs)), 0)
    rb_no <- round(runif(2,1,NROW(rbs)), 0)
    wr_no <- round(runif(4,1,NROW(wrs)), 0)
    tes_no <- round(runif(1,1,NROW(tes)), 0)
    def_no <- round(runif(1,1,NROW(def)), 0)
    
    qb_name <- qbs[qb_no]
    rb_name <- rbs[rb_no]
    wr_name <- wrs[wr_no]
    tes_name <- tes[tes_no]
    def_name <- def[def_no]
    
    # Save the output
    out <- c(qb_name, rb_name, wr_name, tes_name, def_name)
    out2 <- subset(dat, full_name %in% out & Week == 5)
    out_sal <- sum(out2$DK.salary)
  }
  
  # Create the season long data for the roster
  dat2 <- subset(data, full_name %in% out2$full_name)
  
  # Create the week by week averages
  wbw_avg <- aggregate(DK.points ~ Week, data = dat2, FUN = sum2)

  # Calculate the risk/return for the portfolio
  rost_mean <- mean2(wbw_avg$DK.points)
  rost_sd <- sd2(wbw_avg$DK.points)
  
  # Return a portfolio and data points
  roster <- paste0(out2$full_name, collapse = " ")
  out3 <- data.frame("roster" = roster, "mu" = rost_mean, "sigma" = rost_sd)
  return(out3)
}

# Create a 100 roster data set
n <- 10000
roster_space <- matrix(NA, nrow = n, ncol = 3)
roster_space <- as.data.frame(roster_space)
names(roster_space) <- c("roster","mu","sigma")
pb <- txtProgressBar(1,n, style = 3)
for(j in 1:n){
  # Grab a sample roster
  pot <- sample_roster()
  
  # Save its qualities
  roster_space$roster[j] <- as.character(pot$roster)
  roster_space$mu[j] <- pot$mu
  roster_space$sigma[j] <- pot$sigma
  
  # Update the progress bar
  setTxtProgressBar(pb, j)
}
close(pb)

plot(roster_space$mu ~ roster_space$sigma, ylim = c(0,200), xlim = c(0, 200))
abline(h = mean(roster_space$mu))

# scale risk and reward
roster_space$mu_prime <- roster_space$mu/max(roster_space$mu)
roster_space$sigma_prime <- roster_space$sigma/max(roster_space$sigma)

plot(roster_space$mu_prime ~ roster_space$sigma_prime)
abline(h = mean(roster_space$mu_prime))

# function for euclidian distance
eu_dist <- function(x,y){
  x2 <- x^2
  y2 <- y^2
  out <- sqrt(x2 + y2)
}

# calculate euclidian distance for risk/reward
roster_space$dist <- NA
pb <- txtProgressBar(1,n, style = 3)
for(k in 1:nrow(roster_space)){
  # calculate the distance
  roster_space$dist[k] <- eu_dist(roster_space$sigma_prime[k],roster_space$mu_prime[k])
  
  # Update the progress bar
  setTxtProgressBar(pb, j)
}
close(pb)

z <- which.max(roster_space$dist)
points(roster_space$sigma_prime[z], roster_space$mu_prime[z], col = "red")

