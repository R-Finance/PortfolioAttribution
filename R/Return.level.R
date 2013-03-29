#' aggregates portfolio returns up to the chosen level from the hierarchy
#' 
#' Aggregates returns and weights up to the chosen level from the hierarchy.
#' Hierarchy can be used from the \code{buildHierarchy} function or 
#' defined manually in the same way as the \code{buildHierarchy}'s 
#' output. If for the selected level the values in the hierarchy are numeric, 
#' the aggregation of returns or weights is performed by quintiles.
#' \code{Weight.transform} makes transformation of weights to the xts object
#' conformable with returns.
#'
#' @aliases Return.level
#' @param Rp xts, data frame or matrix of portfolio returns
#' @param wp vector, xts, data frame or matrix of portfolio weights
#' @param h  data.frame with portfolio hierarchy
#' @param level level from the hierarchy to which returns and weights will be 
#' aggregated
#' @author Andrii Babii
#' @seealso  \code{buildHierarchy} \cr \code{\link{Attribution}} \cr 
#' \code{\link{Weight.level}}
#' @references Christopherson, Jon A., Carino, David R., Ferson, Wayne E.  
#' \emph{Portfolio Performance Measurement and Benchmarking}. McGraw-Hill. 
#' 2009. Chapter 17
#' @keywords attribution
#' @examples
#' 
#' data(attrib)
#' Return.level(Rp = attrib.returns[, 1:10], wp = attrib.weights[1, ], h = attrib.hierarchy, level = "MarketCap")
#' 
#' @export
Return.level <-
function(Rp, wp, h, level = "Sector")
{   # @author Andrii Babii
  
    # DESCRIPTION:
    # Function to aggregate returns up to the chosen level from the hierarchy
    
    # Inputs:
    # Rp      xts, data frame or matrix of portfolio returns
    # wp      vector, xts, data frame or matrix of portfolio weights
    # h       data.frame with portfolio hierarchy
    # level   level from the hierarchy to which the aggregation will be done
  
    # Outputs: 
    # This function returns portfolio returns at the chosen level
  
    # FUNCTION:
    # Transform data to the xts objects    
    Rp = checkData(Rp, method = "xts")
    wp = Weight.transform(wp, Rp)
    
    # If level has numeric values we replace numeric values by quintiles
    if (is.numeric(h[[level]])){
      h = HierarchyQuintiles(h, level)
    }
    h = split(h$primary_id, h[level])
    returns = as.xts(matrix(NA, ncol = length(h), nrow = nrow(Rp)), index(Rp))
    for(i in 1:length(h)){
      returns[, i] = rowSums(Rp[, h[[i]]] * coredata(wp[, h[[i]]]))
    }
    colnames(returns) = names(h)
    return(returns)
}