# Leslie Huang

### Set up the workspace
rm(list=ls())
setwd("/Users/lesliehuang/un-topic-model/")

# load the models
load("un_models.RData")

set.seed(1234)

libraries <- c("foreign", "utils", "dplyr", "plyr", "devtools", "quanteda", "ggplot2", "topicmodels", "lda", "ldatuning", "LDAvis", "stringi", "plotrix")
lapply(libraries, require, character.only=TRUE)

# Get topic proportions over docs
topics_over_docs <- as.data.frame(model_75@gamma)

# Add substantive labels for topics
topic_labels <- read.csv("terms_m75_labeled.csv", stringsAsFactors = FALSE)
topic_names <- paste("Topic", topic_labels$X.2, topic_labels$X.1)
colnames(topics_over_docs) <- topic_names

# Add country, year
doc_vars <- docvars(un_corpus)

topics_over_docs$year <- doc_vars$year 
topics_over_docs$country <- doc_vars$country

# Topic means by country

country_topic_means <- topics_over_docs %>% group_by(country) %>% summarise_all("mean")

country_topic_ses <- topics_over_docs %>% group_by(country) %>% summarise_all(funs(std.error))

# Generate plots
# 
# for (i in 2:76) {
#  plot(
#       country_topic_means[[i]], 
#       main = paste("Country means on", colnames(country_topic_means)[i], sep = " "),
#       xlab = "Country",
#       ylab = "Mean proportion of documents about topic"
#       )
#   text(country_topic_means[[i]], country_topic_means$country, cex = 0.5, pos = 4, col = "red") 
#   
# }

# Generate graphs

generate_graph <- function(topic_num) {
  
  dat <- data.frame(cbind(country_topic_means$country, 
                          country_topic_means[[topic_num]], 
                          country_topic_ses[[topic_num]]), 
                    stringsAsFactors = FALSE
                    )
  
  colnames(dat) <- c("country", "mean", "se")

  title <- paste("Mean by Country for", colnames(country_topic_means)[topic_num], sep = " ")
  
  g <- ggplot(dat, aes(x = country, y = as.numeric(mean)) ) + 
    geom_point() + 
    ggtitle(title) +
    ylab("Mean proportion of documents about topic") +
    xlab("Country") +
    geom_errorbar(aes(ymin = as.numeric(mean) - as.numeric(se), ymax = as.numeric(mean) + as.numeric(se)), width = 0.1) +
    scale_x_discrete(labels = dat$country) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(g)
  ggsave(paste(title, ".png", collapse = "", sep = ""), width = 15, height = 8, units = "in")
  
}

# Generate and save the graphs
for (i in 2:76) {
  generate_graph(i)
}

