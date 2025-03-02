library(rattle)
library(xtable)
library(ggplot2)
library(MASS)
library(reshape2)
library(gridExtra)
library(MASS)
library(reshape2)
library(gridExtra)
library(caret)
library(e1071)


wine$Type <- as.factor(wine$Type)


set.seed(123)
# 70% treino, 30% teste
trainIndex <- createDataPartition(wine$Type, p = .7, list = FALSE, times = 1)
wineTrain <- wine[trainIndex, ]
wineTest <- wine[-trainIndex, ]

distances <- dist(wineTrain[, -1], method = "euclidean")

# Aplicar MDS para visualizar os grupos
mds <- cmdscale(distances, k = 2) 
mds_data <- as.data.frame(mds)
mds_data$Type <- wineTrain$Type

mds_plot <- ggplot(mds_data, aes(x = V1, y = V2, color = Type)) +
  geom_point(size = 3) +
  labs(title = "MDS dos Dados Wine",
       x = "Primeira Dimensao",
       y = "Segunda Dimensao") +
  theme_minimal()

print(mds_plot)

# Se quisermos visualizar como o SVM separa os grupos com base nas variáveis Phenols e Ash, podemos fazer que 


modelo <- svm(Type ~., data = wineTrain,
              kernel = "linear",
              cost = 1)

plot(modelo, wineTrain, formula = Phenols ~ Ash)


# O formato de X indica que essas variáveis são os vetores de suporte, enquanto que a cor deles indica a qual grupo pertence. Analogamente, a cor do plano de fundo indica a qual grupo uma variável que está na região será designado.    

  

# Para visualizar como ficaram os grupos formados pelo SVM e como eles são na realidade, podemos aplicar um MDS, assim, temos que:
  

distances <- dist(wineTest[, -1], method = "euclidean")

mds <- cmdscale(distances, k = 2)

mds_data <- as.data.frame(mds)
mds_data$ajustado <- predict(modelo, wineTest[,-1])

mds_plot <- ggplot(mds_data, aes(x = V1, y = V2, color = ajustado)) +
  geom_point(size = 3) +
  labs(title = "MDS dos valores previstos usando dados de teste",
       x = "Primeira Dimensao",
       y = "Segunda Dimensao") +
  theme_minimal()
mds_plot2 <- ggplot(mds_data, aes(x = V1, y = V2, color = wineTest$Type)) +
  geom_point(size = 3) +
  labs(title = "MDS dos valores reais dados teste",
       x = "Primeira Dimensao",
       y = "Segunda Dimensao") +
  theme_minimal()

grid.arrange(mds_plot, mds_plot2, ncol = 2)



# A qualidade do ajuste pode ser verificada nessa matriz de confusão, onde podemos notar que o ajuste foi muito bem feito


cm <- confusionMatrix(mds_data$ajustado, wineTest$Type)
cm$table

# E se fizermos LDA, chegamos ao seguinte resultado


linear <- lda(Type~., wineTrain)
p <- predict(linear, wineTest)
p2 <- predict(linear, wineTest)$class
tab1 <- table(Predicted = p2, Actual = wineTest$Type)

tab1
