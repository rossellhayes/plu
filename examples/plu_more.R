plu::more(letters)

# Setting `max`
plu::more(letters, max = 10)
plu::more(letters, max = 27)

# If `max` is Inf or NULL, all elements will be preserved
plu::more(letters, max = Inf)

# If `max` is less than one, no elements will be preserved
plu::more(letters, max = 0)

# Setting element type
plu::more(letters, type = "letter")

# If `type` is FALSE or NULL, no type will be included
plu::more(letters, type = FALSE)

# Automatically generating type
plu::more(1:100)
plu::more(as.list(1:100))
plu::more(c(as.list(1:2), as.list(letters)))
plu::more(fracture::fracture((1:9) / (9:1)))

# Setting a determiner other than "more"
plu::more(letters, det = "other")

# Applying a function to the number
plu::more(letters, fn = nombre::cardinal)

# Automatic pluralization of type
fish <- c("sea bass", "crucian carp", "dace", "coelecanth")
plu::more(fish, max = 3, type = "fish")
plu::more(fish, max = 2, type = "fish")

teeth <- c("incisor", "canine", "molar", "wisdom tooth")
plu::more(teeth, max = 3, type = "tooth")
plu::more(teeth, max = 2, type = "tooth")

cacti <- c("saguaro", "prickly pear", "barrel", "star")
plu::more(cacti, max = 3, type = "cactus")
plu::more(cacti, max = 2, type = "cactus")

# Using plu_more() within a function
verbose_sqrt <- function(x) {
  if (any(x < 0)) {
    problems <- x[x < 0]
    prob_msg <- crayon::silver(encodeString(problems, quote = "`"))

    warning(
      "Square root is undefined for ",
      and::and(plu::more(prob_msg, fn = crayon::silver, type = "input.")),
      call. = FALSE
    )
  }

  sqrt(x)
}

ints <- round(runif(20, -10, 10))
verbose_sqrt(ints)
