

guess_columns <- function(df) {
  
  cnms <- colnames(df)
  n_cn <- length(cnms)
  
  typs <- rep("", n_cn)
  for (j in 1:n_cn) {
    typs[j] <- get_col_type(df, cnms[j]) 
  }
  
  rl <- list(nums = NULL, facs = NULL, oths = NULL)
  rl$nums <- cnms[typs %in% "num"]
  rl$facs <- cnms[typs %in% "fac"]
  rl$oths <- cnms[typs %in% "???"]
  
  return(rl)
  
}


get_col_type <- function(df, col_nm) {
  
  col_as_vec <- df[[col_nm]]
  col_class <- class(col_as_vec)
  # print(col_class)
  
  if (any(c("numeric", "integer") %in% col_class)) {
    return("num")
  } else if (any(c("character", "factor", "logical") %in% col_class)) {
    return("fac")
  } else {
    return("???")
  }
  
}

# test_df <-
#   data.frame(
#     a = as.integer(1:5),
#     b = TRUE,
#     c = seq(1.5, 6.5, length = 5),
#     d = LETTERS[1:5],
#     e = factor(LETTERS[1:5]),
#     f = complex(imaginary = 1:5),
#     stringsAsFactors= FALSE
#   )
# 
# 
# get_col_type(test_df, "a") 
# get_col_type(test_df, "b") 
# get_col_type(test_df, "c") 
# get_col_type(test_df, "d") 
# get_col_type(test_df, "e") 
# get_col_type(test_df, "f") 
# guess_columns(test_df) 

