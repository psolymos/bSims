get_table <- function(x, type=c("removal", "visits"))
  switch(match.arg(type),
    "removal"=x$removal,
    "visits"=x$visits)
