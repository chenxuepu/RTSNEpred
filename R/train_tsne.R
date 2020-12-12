
#' Pipe operator
#'
#' See \code{\link[magrittr]{%>%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' train tsne but use MDS to Y_init,and keep pca information in order to predict.
#'
#'
#' @param data matrix; Data matrix (each row is an observation, each column is a variable)
#' @param ... Other arguments that can be passed to Rtsne(except check_duplicates and Y_init)
#'
#' @return List with the following elements:
#' \item{Y}{Matrix containing the new representations for the objects}
#' \item{N}{Number of objects}
#' \item{origD}{Original Dimensionality before TSNE (only when \code{X} is a data matrix)}
#' \item{perplexity}{See above}
#' \item{theta}{See above}
#' \item{costs}{The cost for every object after the final iteration}
#' \item{itercosts}{The total costs (KL-divergence) for all objects in every 50th + the last iteration}
#' \item{stop_lying_iter}{Iteration after which the perplexities are no longer exaggerated}
#' \item{mom_switch_iter}{Iteration after which the final momentum is used}
#' \item{momentum}{Momentum used in the first part of the optimization}
#' \item{final_momentum}{Momentum used in the final part of the optimization}
#' \item{eta}{Learning rate}
#' \item{exaggeration_factor}{Exaggeration factor used to multiply the P matrix in the first part of the optimization}
#' \item{Y_init}{the MDS value to Y_init}
#' \item{pca_coef}{we use pca to measuring MDS, we keep the value for predict}
#' \item{center}{the data center, predict needs to use it}
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
#' iris_pred <- predict(object = iris_tsne,data = iris_test[,1:4],k = 3) %>%
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
train_tsne <- function(data,...){
  ## the function is use by myself , so it has not check anything
  pca <- stats::prcomp(data)
  Y_init <- as.matrix(pca$x[,1:2])

  tsne <- Rtsne::Rtsne(data,check_duplicates = F,Y_init = Y_init,... = ...)
  tsne[["Y_init"]] <- Y_init
  tsne[["pca_coef"]] <- as.matrix(pca$rotation[,1:2])
  tsne[["center"]] <- pca$center
  class(tsne) <- "tsne"
  tsne
}


#' @export
predict.tsne <- function(object, data, k = 5,...){
  ## the function is use by myself , so it has not check anything
  if(class(object)!="tsne"){
    stop("class is not TSNE")
  }
  data_mean <- matrix(object$center,nrow = nrow(data),ncol = ncol(data),byrow = T)

  RS <- object$Y-object$Y_init

  data_pca_value <- as.matrix(data-data_mean) %*% object$pca_coef

  data_predict <- matrix(nrow = nrow(data),ncol = 2) %>%
    as.data.frame() %>%
    `names<-`(c("x","y"))

  for(i in 1:nrow(data)){
    data_dist_order <- (object$Y_init-matrix(data_pca_value[i,],ncol = 2,nrow = nrow(object$Y_init),byrow = T))^2 %>% rowSums() %>% order()
    index <- data_dist_order[1:k]
    move <- RS[index,] %>% colMeans()
    data_predict[i,] <- data_pca_value[i,]+move
  }
  data_predict
}

