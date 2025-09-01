# Installation
# -------------
# install.packages('bnlearn')
# install.packages('gRain')

library(bnlearn)
library(gRain)

# Function that runs a list of queries on a given model 
runQueries <- function(bn, queries) {
  lapply(queries, function(q) {
    # Runs a single query on the given model
    querygrain(bn,
               nodes = q$conditioned,
               evidence = q$conditional)
  })
}

# Function that zips queries and results
zipped <- function(qs, rs) {
  Map(function(q, r) {
    list(query = q, answer = r[1])
  }, qs, rs) 
}

# Function that prints results
printAnswers <- function(net, queries) {
  cat("\n=============================\n")
  cat(deparse(substitute(net))) # Print the name of the net
  cat("\n=============================\n")

  answers <- runQueries(net, queries)
  invisible(
    lapply(zipped(queries, answers), function(x) {
      cat("\n-------------\n")
      cat("Query:\n\n")
      cat("Conditioned:\n")
      print(x$query$conditioned[[1]])
      cat("Conditional:\n")
      print(x$query$conditional)
      cat("\n")
      cat("Answer:\n")
      print(x$answer[[1]])
    })
  )
}


# Networks (assumes networks are in working directory)
alarm <- as.grain(read.net("alarm.net"))
asia <- as.grain(read.net("asia.net"))


# Queries
alarmQueries <- list(
  list(conditioned = "HRBP", conditional = list(HR = "LOW")),
  list(conditioned = "HRBP", conditional = list()),
  list(conditioned = "ARTCO2", conditional = list(EXPCO2 = "LOW")),
  list(conditioned = "ARTCO2", conditional = list())
)
asiaQueries <- list(
  list(conditioned = "tub",
       conditional = list(asia = "yes")),
  list(conditioned = "either",
       conditional = list(asia = "yes", smoke = "no", bronc = "yes")),
  list(conditioned = "either",
       conditional = list(asia = "yes", tub = "yes", smoke = "no", bronc = "yes")),
  list(conditioned = "either",
       conditional = list(asia = "yes", tub = "no", smoke = "no", bronc = "yes")),
  list(conditioned = "dysp",
       conditional = list(tub = "yes", lung = "yes"))
)


printAnswers(alarm, alarmQueries)
printAnswers(asia,  asiaQueries)

# Manual Query examples, using P(A && B | C)

# P2
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(asia = "yes"),
           type = "joint")
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(bronc = "no", lung = "yes"),
           type = "joint")
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(bronc = "no", lung = "yes", dysp = "yes"),
           type = "joint")
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(bronc = "no", lung = "yes", xray = "yes"),
           type = "joint")
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(bronc = "no", lung = "yes", xray = "yes", dysp = "yes"),
           type = "joint")
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(bronc = "no", lung = "yes", xray = "yes", dysp = "no"),
           type = "joint")


# P3
querygrain(asia,
           nodes = c("xray", "dysp"),
           evidence = list(asia = "yes"),
           type = "joint")


