#Fist running 
install.packages("class")
install.packages("ggplot2")


# Get and print current working directory.
print(getwd())
# Set current working directory.
setwd("C:/Users/Eonassis-Air/Desktop/Doutorado UFA/3 SM Reg 2022 1/projetoIA")
# Get and print current working directory.
print(getwd())

#read csv MB
DATA_HM <- read.csv("DATA_HM.csv")
#print data
print(DATA_HM)

#verify is NA ?
any(is.na(DATA_HM))

#train data
hm_train <- DATA_HM[1:45, -3]
#train data labels
hm_train_labels <- DATA_HM[1:45, 3]

#test data
hm_test <- DATA_HM[46:55, -3]
#test data labels
hm_test_labels <- DATA_HM[46:55, 3]

library(class)

#knn alterar o k para achar melhor resultado para o dataset
hm_test_pred <- knn(train = hm_train, test = hm_test, cl = hm_train_labels, k=5)

# unir o vetor de resultados e o de teste
hm_res <- data.frame(hm_test_pred, hm_test_labels)

hm_res$results = ifelse(hm_res$hm_test_pred == hm_res$hm_test_labels, 'Ok',
                      ifelse(hm_res$hm_test_pred != hm_res$hm_test_labels, 'Erro', 'Erro'))

hm_res$results_data = ifelse(hm_res$hm_test_pred == hm_res$hm_test_labels, '1',
                        ifelse(hm_res$hm_test_pred != hm_res$hm_test_labels, '2', '2'))


# imprime o resultado
print(hm_res)

######################plot########################

# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("ok", "Erro"),
  count=c(sum(hm_res==1), sum(hm_res==2))
)


# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart

######## best k ###############


hm_resfor <- hm_res

for (i in 1:45) {
  
  #knn alterar o k para achar melhor resultado para o dataset
  hm_test_predfor <- knn(train = hm_train, test = hm_test, cl = hm_train_labels, k=i)
  
  
  # unir o vetor de resultados e o de teste
  hm_resfor <- data.frame(hm_resfor , hm_test_predfor)
  
  
  
}

hm_resfor
write.csv(hm_resfor, "aaa.csv")
