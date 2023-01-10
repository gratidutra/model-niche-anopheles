#devtools::install_github("danlwarren/ENMTools")
library(ENMTools)
library(tidyverse)
library(tensorflow)
library(keras)


source('src/functions.R')


# ENMTools ----------------------------------------------------------------


env.files <- list.files(path = "data/workflow_maxent/an_albimanus/G_Variables/Set_2/Current", pattern = "PC", full.names = TRUE)
env <- stack(env.files)
names(env) <- c("PC01", "PC02", "PC03", "PC04")
env <- setMinMax(env)

plot(env[[1]])

env <- check.env(env)

an_albimanus <- enmtools.species()
an_albimanus

an_albimanus.path <-"data/workflow_maxent/an_albimanus/an_albimanus_joint.csv"
an_albimanus <- enmtools.species(species.name = "an_albimanus", 
                              presence.points = read_csv(an_albimanus.path) %>% 
                                select(longitude, latitude) %>%
                                rename(Longitude = longitude, Latitude = latitude)) 
an_albimanus$range <- background.raster.buffer(an_albimanus$presence.points, 25000, mask = env)
an_albimanus$background.points <- background.points.buffer(points = an_albimanus$presence.points,
                                                        radius = 20000, n = 1000, mask = env[[1]])

an_albimanus <- check.species(an_albimanus)
interactive.plot.enmtools.species(an_albimanus)

an_albimanus.glm <- enmtools.glm(species = an_albimanus, env = env, f = pres ~ PC01 + PC02 + PC03 + PC04, test.prop = 0.3)
#an_albimanus.glm

#enmtools.rf(an_albimanus, env = env)

dataset_to_keras <- an_albimanus.glm[["analysis.df"]]

write_csv(dataset_to_keras, 'dataset_to_keras.csv')


# train_test --------------------------------------------------------------
set.seed(123)

dataset_to_keras <- read.csv('dataset_to_keras.csv')

spec = c(train = .7, test = .3)

g = sample(cut(
  seq(nrow(dataset_to_keras)), 
  nrow(dataset_to_keras)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(dataset_to_keras %>% select(-Longitude,-Latitude), g)

train <- res$train
#test <- res$test
val <- res$test

#ggplot(train, aes(x = PC02, y = PC01, col = presence)) + 
#  geom_point(alpha = .2)

# keras -------------------------------------------------------------------
feature_names <- colnames(train) %>% setdiff("presence")

model <- keras_model_sequential(input_shape = c(length(feature_names))) %>%
  #layer_flatten() %>%
  layer_dense(512, activation = "relu") %>%
  layer_dense(256, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(256, activation = "relu") %>%
  layer_dropout(0.3) %>%
  layer_dense(1, activation = "sigmoid")

model

metrics <- list(
  metric_auc(name = 'auc'),
  metric_precision(name = "precision"),
  metric_recall(name = "recall")
)
model %>% compile(
  optimizer = optimizer_adam(0.025),
  loss = "binary_crossentropy",
  metrics = metrics
)

dir_create('callbacks')

callbacks <- list(
  callback_model_checkpoint("callbacks/an_albimanus_at_epoch_{epoch}.h5"))

train_features <- as.matrix(train[feature_names])
train_targets <- as.matrix(train$presence)
validation_data <- list(
  as.matrix(val[feature_names]),
  as.matrix(val$presence))

model %>%
  fit(train_features, train_targets,
      validation_data = validation_data,
      batch_size = 2048, epochs = 100,
      callbacks = callbacks,
      verbose = 2)