source("requirements.R")

elite <- read_csv("Data/boardgame-elite-users.csv")
frequent <- read_csv("Data/boardgame-frequent-users.csv")
allusers <- read_csv("Data/boardgame-users.csv")

titles <- read_csv("Data/boardgame-titles.csv")
titles <- titles %>% rename(gameID = `boardgamegeek.com game ID`)

elite <- elite %>% group_by(gameID) %>% 
        mutate(avg_rating = mean(rating), freq = n()) %>%
        left_join(titles) %>%
        rename(userID = `Compiled from boardgamegeek.com by Matt Borthwick`)

df <- acast(elite, userID~gameID, value.var="rating") 
df <- as.data.frame(cbind(as.integer(rownames(df)), df))

#write_csv(as.data.frame(df), "C:/Users/Daniel/Google Drive/PDXDS-RecEngines/sparsematrix.csv")

ncomp <- estim_ncpPCA(df[,2:ncol(df)], ncp.min = 0, ncp.max = 6)

df.imp <- imputePCA(df[,2:ncol(df)], ncp = 6, scale = TRUE,
                     method = "EM", row.w = NULL, coeff.ridge = 1,
                     threshold = 1e-06, seed = NULL, nb.init = 1, maxiter = 1000)

dfPCA <- prcomp(formula = ~., data = df[,2:ncol(df)], rank. = 5, retx = TRUE, na.action = na.exclude)

PCAEM <- prcomp(df.imp$completeObs, rank. = 6)

#####MISC PLOTTING#####
hist(elite[elite$gameID == 9216,]$rating)
ggplot(elite, aes(x = freq, y = avg_rating))+geom_line()
####################

alltitles = unique(elite$gameID)







