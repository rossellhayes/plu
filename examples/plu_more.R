plu::more(letters)

# Setting `max`
plu::more(letters, max = 10)
plu::more(letters, max = 27)

# If `max` is Inf or NULL, all elements will be preserved
plu::more(letters, max = Inf)

# If `max` is less than one, no elements will be preserved
plu::more(letters, max = 0)

# Setting `type`
plu::more(letters, type = "letter")

# If `type` is FALSE or NULL, no type will be included
plu::more(letters, type = FALSE)

# Automatically generating type
plu::more(1:100)
plu::more(as.list(1:100))
plu::more(fracture::fracture((1:9) / (9:1)))

# Applying a function to the number
plu::more(letters, fn = nombre::cardinal)
message(plu::more(sapply(letters, crayon::blue), fn = crayon::blue))

# Automatic pluralization of type
fish <- c("sea bass", "pale chub", "crucian carp", "dace", "coelecanth")
plu::more(fish, max = 3, type = "fish")
plu::more(fish, max = 4, type = "fish")

teeth <- c("incisor", "canine", "premolar", "molar", "wisdom tooth")
more(teeth, max = 3, type = "tooth")
more(teeth, max = 4, type = "tooth")

cacti <- c("saguaro", "prickly pear", "barrel", "star", "chin")
more(cacti, max = 3, type = "cactus")
more(cacti, max = 4, type = "cactus")

# Formatting with `sep`, `conj`, and `oxford`
more(letters, conj = " or ", det = "other")

more(letters, oxford = FALSE)
more(letters, oxford = TRUE)

more(letters, sep = "; ", oxford = TRUE)
more(letters, sep = " + ", conj = " + ... + ")

# Using plu_more() within a function
verbose_sqrt <- function(x) {
  if (any(x < 0)) {
    problems <- crayon::silver(encodeString(x[x < 0], quote = "`"))

    warning(
      "Square root is undefined for ",
      plu_more(problems, fn = crayon::silver, type = "input."),
      call. = FALSE
    )
  }

  suppressWarnings(sqrt(x))
}

verbose_sqrt(-10:10)
