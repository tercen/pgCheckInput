# Constructor function for the class
#' @import reshape
#' @import plyr
#' @export
pgError <- function(title, msg = "", url = "", msgFun = NULL)  {
  # Do something here with x, args and put in something
  object <- list(title = title, url = url, msgFun = msgFun)
  class(object) <- "pgError"
  return (object)
}

#' @export
check = function(object, data, ...){
  UseMethod("check", object)
}

#' @export
check.pgError = function(object, x, openUrlOnError = "FALSE", ...){
  msg = object$msgFun(x, ...)
  if(!is.null(msg)){
      if(openUrlOnError){
        try(openURL(object))
      }
      stop(paste(object$title,": ", msg, " Check the PamCloud for more information on this error message."))
  }
}

#' @export
openURL= function(object){
  UseMethod("openURL", object)
}

#' @export
openURL.pgError = function(object){
  browseURL(object$url)
}

#defined checks
#' @export
EmptyCells   = pgError(title = "Missing values are not allowed",
                       url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Missing%20values%20are%20not%20allowed",
                       msgFun = function(aCube){
                         msg = "The data contains missing values,
                                make sure that the Cross-tab Window does
                                not contain any empty Data Cells."

                         aFrame = pData(aCube)
                         X = cast(aFrame, rowSeq ~ colSeq, value = "value")
                         if(any(is.na(X))){
                           return(msg)
                         }else{
                           return(NULL)
                         }

                       })

#' @export
MultipleValuesPerCell = pgError(title = "Multiple values per Data Cell are not allowed",
                                msgFun = function(aCube){
                                  msg = "make sure that every Data Cell in the Cross-tab Window contains not more than a single value."
                                  count = cast(pData(aCube), rowSeq~colSeq, value = "value", fun.aggregate = length)
                                  if(any(count[,-1]>1)){
                                      return(msg)
                                    } else {
                                      return(NULL)
                                    }
                                  },
                                url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Multiple%20values%20per%20Data%20Cell%20are%20not%20allowed"
                          )

#' @export
RowsWithConstantValue = pgError(title = "Rows with identical values in all the cells are not allowed",
                                msgFun = function(aCube){
                                  msg = "Please make sure that the input data does not contain rows with zero standard deviation."
                                  a = daply(pData(aCube), ~ rowSeq, .fun = function(aFrame)return(sd(aFrame$value)))
                                  if(any(a==0) ){
                                      return(msg)
                                    }else{
                                      return(NULL)
                                    }
                                  }, url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Rows%20with%20identical%20values%20in%20all%20the%20cells%20are%20not%20allowed"
                                )
#' @export
ExactNumberOfFactors = pgError(title = "Incorrect number of factors",
                                  msgFun = function(aCube, groupingType, nFactors = 1, altGroupingName = NULL){
                                    metaData = varMetadata(aCube)
                                    factors =  colnames(pData(aCube))[metaData$groupingType== groupingType]
                                    if (length(factors) != nFactors){
                                      if (is.null(altGroupingName)){
                                        groupingName = groupingType
                                      } else {
                                        groupingName = altGroupingName
                                      }
                                      msg = paste("Exactly ", nFactors, " factor(s) are required for: ", groupingName,".", sep ="")
                                    } else {
                                      msg = NULL
                                    }
                                    return(msg)
                                   }, url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Incorrect%20Number%20of%20Factors")

#' @export
FactorPresent = pgError(title = "Factor not present",
                        msgFun = function(aCube, groupingType, altGroupingName = NULL){
                          metaData = varMetadata(aCube)
                          factors =  colnames(pData(aCube))[metaData$groupingType== groupingType]
                          if(length(factors) == 0){
                            if (is.null(altGroupingName)){
                              groupingName = groupingType
                            } else {
                              groupingName = altGroupingName
                            }
                            msg = paste("At least one factor is required for: ", groupingName,".", sep ="")
                          } else {
                            msg = NULL
                          }
                          return(msg)
                        },
                        url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Factor%20Not%20Present"
                        )
#' @export
FactorIsNumeric = pgError(title = "Factor must be numeric",
                     msgFun = function(aCube, factorName){
                       metaData = varMetadata(aCube)
                       aFactor = metaData$labelDescription[metaData$labelDescription == factorName]
                       if (!is.numeric(data[[aFactor]] ) ){
                         msg = paste("Factor:", aFactor, "must be numeric.")
                       } else {
                         msg = NULL
                       }
                       return(msg)
                      },
                       url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Factor%20must%20be%20numeric"
                     )

#' @export
ExactNumberOfGroups = pgError(title = "Incorrect number of groups",
                                  msgFun = function(aCube, factorName, nLevels){
                                  fac = as.factor(aCube[[factorName]])
                                  if(length(levels(fac)) != nLevels){
                                    msg = paste("Factor:",factorName, "should contain exactly", nLevels, "groups.")
                                  }else{
                                    msg = NULL
                                  }
                                  return(msg)
                                },url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Incorrect%20number%20of%20groups"
                              )
#' @export
MultipleGroupsPresent = pgError(title = "Factor must contain multiple groups",
                                msgFun = function(aCube, factorName){
                                  fac = as.factor(aCube[[factorName]])
                                  if(length(levels(fac)) < 2){
                                    msg = paste("Factor:",factorName, "should contain at least 2 groups.")
                                  }else{
                                    msg = NULL
                                  }
                                  return(msg)
                                }, url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Factor%20must%20contain%20multiple%20groups"

                                )
#' @export
NonUniqueDataMapping = pgError(title = "Input data is not unique",
                               msgFun = function(aCube){
                                 sID = pData(aCube)$sids
                                 if ( length(sID) != length(unique(sID)) ){
                                   msg = "Invalid Mapping. Not all data points are unique"
                                 } else {
                                   msg = NULL
                                 }
                                 return(msg)
                               },
                                 url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=Input%20data%20is%20not%20unique"
                               )
#' @export
PamCloudInstalled = pgError(title = "PamCloud package not installed",
                            msgFun = return("The required R-package PamCloud has not been (correctly) installed. Please check the PamCloud R-package page on the PamWiki for more information"),
                            url = "https://pamcloud.pamgene.com/wiki/Wiki.jsp?page=PamCloud%20package%20not%20installed"
                            )



