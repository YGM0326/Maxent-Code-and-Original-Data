
# 加载需要的包
install.packages("reshape2")
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)

# Replace the following path to match the location where your csv file is stored.
df <- read.csv("C:\\Users\\10284\\Desktop\\damage\\data\\Dis_Village.csv")
#According to the model classification, based on the maxent 10 times repeated results and calculate the average and error of x and y

df$mean_x <- rowMeans(df[c("x1", "x2", "x3", "x4", "x5","x6", "x7", "x8", "x9", "x10")])
df$mean_y <- rowMeans(df[c("y1", "y2", "y3", "y4", "y5","y6", "y7", "y8", "y9", "y10")])

#Calculate the error of y, which requires your accurate equation, here we assume this is the standard deviation of y
df$y_error <- apply(df[c("y1", "y2", "y3", "y4", "y5","y6", "y7", "y8", "y9", "y10")], 1, sd)


# Create a new data frame, includes model, X mean, Y mean and Y error.
df_new <- df %>% 
  select(model, mean_x, mean_y, y_error)

#  Display new data frame
print(df_new)
df_long <- df_new %>%
  tidyr::pivot_longer(c(mean_x, mean_y, y_error), names_to = "measurement", values_to = "value")

dataframe<-df_new
write.csv(dataframe, "C:\\Users\\10284\\Desktop\\habitat\\GK\\mydata2.csv", row.names = FALSE)


ggplot(df, aes(x=mean_x, y=mean_y, color=model, fill=model)) +
  geom_ribbon(aes(ymin=mean_y-y_error, ymax=mean_y+y_error), alpha=0.5,colour=NA) +
  geom_line() +
  labs(x="Distance to village road (m)", y="Wild boar damage risk") +
  theme_classic()+theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),axis.line = element_line(size = 0.5),legend.position = "none" 
  )
p<-ggplot(df, aes(x=mean_x, y=mean_y, color=model, fill=model)) +
  geom_ribbon(aes(ymin=mean_y-y_error, ymax=mean_y+y_error), alpha=0.5,colour=NA) +
  geom_line() +
  labs(x="Distance to village road (m)", y="Wild boar damage risk") +
  theme_classic()+theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 22),axis.line = element_line(size = 0.5),legend.position = "none" 
  )

ggsave("C:\\Users\\10284\\Desktop\\damage\\data\\Distance to village road.pdf", plot = p, width = 7, height = 8)
