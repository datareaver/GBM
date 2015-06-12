#
# Generate code from a gbm tree.
# A lot of this is based on looking at the output of pretty.gbm.tree

# The first function generates code from a single node
#
makeif <- function(data, i,  codetable, varname) {
  vname <- varname[data$splitvar[i]]
  left <- data$left[i]
  right <- data$right[i]
  miss  <- data$missing[i]

  # For each of missing, left, and right we find out if it has
  #  a further split.  If so call this routine recursively.
  #  If not fill in the rule.
  if (data$splitvar[miss] == 0)
    out <- paste(codetable$ifna(vname),
                 codetable$val(data$y[miss]), sep=" ")
  else out <- c(paste(codetable$ifna(vname), codetable$newblock),
                paste(codetable$indent,
                      makeif(data, miss, codetable, varname)),
                codetable$endblock)

  # The < clause
  if (data$splitvar[left] ==0)
    out <- c(out, paste(codetable$elseif(vname, data$y[i]),
                        codetable$val(data$y[left])))
  else out <- c(out, paste(codetable$elseif(vname, data$y[i]),
                           codetable$newblock),
                paste(codetable$indent,
                      makeif(data, left, codetable, varname)),
                codetable$endblock)


  # The > clause
  if (data$splitvar[right] == 0)
    out <- c(out, paste(codetable$elsee,
                        codetable$val(data$y[right])))
  else out <- c(out, paste(codetable$elsee, codetable$newblock),
                paste(codetable$indent,
                      makeif(data, right, codetable, varname)),
                codetable$endblock)
  out
}

#
# Code table for R.  R doesn't like a list member named "else"

rctable <- list(ifna = function(x) paste("if (is.na(data$", x, "))", sep=''),
                elseif = function(x, cut) paste0("else if (data$",x, " < ",
                                                 cut, " )"),
                newblock= " {",
                endblock= "}",
                indent = "  ",
                elsee  = "else",
                val = function(x) paste("score <- score +", x),
                init = function(x) paste ("score <- ", x),
                namechange = function(x) x
)

# Code table for SAS
#
sasctable <- list(ifna = function(x) paste("if (", x, "= .) then"),
                  elseif = function(x, cut) paste("else if (", x, "<=",
                                                  cut, ") then"),
                  newblock= " do;",
                  endblock= "end;",
                  indent = "   ",
                  elsee  = "else",
                  val = function(x) paste("score = score +", x, ";"),
                  init = function(x) paste("score = ",  x, ";"),
                  namechange = function(x) gsub('.', '_', x, fixed=TRUE)
)
#

# Code table for Blaze
#
blazectable <- list(ifna = function(x) paste("if (", x, "= unknown) then {"),
                    elseif = function(x, cut) paste("else if (", x, "<=",
                                                    cut, ") then {"),
                    newblock= "",
                    endblock= "}",
                    indent = "   ",
                    elsee  = "else {",
                    val = function(x) paste("score = score +", x, ". }"),
                    init = function(x) paste("score = ",  x, "."),
                    namechange = function(x) {
                      s <- strsplit(x, "\\.")[[1]]
                      paste0(s[1],paste0(toupper(substring(s[-1], 1, 1)), substring(s[-1], 2),
                                         collapse = ""),collapse = "")
                    }
)


#
# Here is the function that the user calls
# fit = a gbm fit
# nodes = a vector of nodes to be used (usually based on gbm.perf)
# codetable = one of the above code tables
#
# Output: a vector of character strings.  Often one would do
#   cat(result, file="somefilename", sep='\n')
# to save it as a text file.
makecode <- function(fit, nodes, codetable=rctable) {
  vname <- sapply(fit$var.names,codetable$namechange,USE.NAMES = FALSE)

  treecode <- function(node, codetable, varname) {
    temp <- data.frame(node[1:5])
    names(temp) <- c("splitvar", "y", "left", "right", "missing")

    # go from base 0 to base 1 indices
    for (i in c(1,3,4,5)) temp[[i]] <- temp[[i]] +1
    makeif(temp, 1, codetable, varname)
  }

  temp <- lapply(fit$trees[nodes], treecode, codetable, vname)
  unlist(temp)
}
