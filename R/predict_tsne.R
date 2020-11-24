#' predict T-SNE
#'
#' use MDS to setup Y_init , then we can predict the T-SNE
#'
#' @param TSNE a list from \link{train_tsne}
#' @param data a data want to pred
#' @param k how many neighbor to use
#'
#' @return
#' a dataframe for 2 dim
#'
#' @export
#'
#' @examples
#' library(RTSNEpred)
#'
#' library(ggplot2)
#' library(magrittr)
#' set.seed(123)
#' iris_index <- sample(1:nrow(iris),size = floor(0.8*nrow(iris)))
#' iris_train <- iris[iris_index,]
#' iris_test <- iris[-iris_index,]
#' set.seed(123)
#' iris_tsne <- train_tsne(iris_train[,1:4])
#' iris_pred <- predict_tsne(TSNE = iris_tsne,test = iris_test[,1:4],k = 3) %>%
#'   as.data.frame() %>%
#'   cbind(iris_test$Species) %>%
#'   `names<-`(c("x","y","Species"))
#' iris_tsne %$%
#'   Y %>%
#'   as.data.frame() %>%
#'   cbind(iris_train$Species) %>%
#'   `names<-`(c("x","y","Species")) %>%
#'   ggplot(aes(x=x,y=y,color = Species)) +
#'   geom_point()
#'
#' ggplot(iris_pred,aes(x=x,y=y,color = Species)) +
#'   geom_point()
#'
#' iris_tsne %$%
#'   Y %>%
#'   as.data.frame() %>%
#'   cbind(iris_train$Species) %>%
#'   `names<-`(c("x","y","Species")) %>%
#'   rbind(iris_pred) %>%
#'   ggplot(aes(x=x,y=y,color = Species)) +
#'   geom_point()
predict_tsne <- function(TSNE, data, k = 5){
  ## the function is use by myself , so it has not check anything
  if(class(TSNE)!="TSNE"){
    stop("class is not TSNE")
  }
  data_mean <- matrix(TSNE$center,nrow = nrow(data),ncol = ncol(data),byrow = T)

  RS <- TSNE$Y-TSNE$Y_init

  data_pca_value <- as.matrix(data-data_mean) %*% TSNE$pca_coef

  data_predict <- matrix(nrow = nrow(data),ncol = 2) %>%
    as.data.frame() %>%
    `names<-`(c("x","y"))

  for(i in 1:nrow(data)){
    data_dist_order <- (TSNE$Y_init-matrix(data_pca_value[i,],ncol = 2,nrow = nrow(TSNE$Y_init),byrow = T))^2 %>% rowSums() %>% order()
    index <- data_dist_order[1:k]
    move <- RS[index,] %>% colMeans()
    data_predict[i,] <- data_pca_value[i,]+move
  }
  data_predict
}
