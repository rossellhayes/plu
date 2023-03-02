plu::ral("apple", pl = FALSE)
plu::ral("apple", pl = TRUE)

plu::ral("apple", n = 1)
plu::ral("apple", n = 2)
plu::ral("apple", n = 0)
plu::ral("apple", n = -1)
plu::ral("apple", n = 0.5)

mon <- c("apple")
tue <- c("pear", "pear")

plu::ral("apple", mon)
plu::ral("pear", tue)

paste("Monday, the caterpillar ate", plu::ral("an apple", mon))
paste("Tuesday, the caterpillar ate", plu::ral("a pear", tue))

paste("Monday, the caterpillar visited", plu::ral("an {apple} tree", mon))
paste("Tuesday, the caterpillar visited", plu::ral("a {pear} tree", tue))

paste("Monday, the caterpillar ate", plu::ral("a {single|multiple} apple", mon))
paste("Tuesday, the caterpillar ate", plu::ral("a {single|multiple} pear", tue))

# Vectorized `n`
foods <- c("apple", "pear", "plum", "strawberry", "orange")
quantities <- c(1, 2, 3, 4, 5)
plu::ral(foods, n = quantities)
paste(
  "The caterpillar ate",
  and::and(paste(nombre::cardinal(quantities), plu::ral(foods, n = quantities)))
)

# Some words have a dual form, a specific form for quantities of two
paste("The caterpillar ate", plu::ral("{the|both|all of the} apple", mon))
paste("The caterpillar ate", plu::ral("{the|both|all of the} pear", tue))
paste("The caterpillar ate", plu::ral("{the|both|all of the} delicacy", foods))

# The string "n" will be replaced by the number used for pluralization
paste("The caterpillar ate", plu::ral("n apple", mon))
paste("The caterpillar ate", plu::ral("n delicacy", foods))

# Special brace strings
plu::ral("{one|two}", n = 1)
plu::ral("{one|two}", n = 2)

plu::ral("{one|two|more}", n = 1)
plu::ral("{one|two|more}", n = 2)
plu::ral("{one|two|more}", n = 3)
plu::ral("{one|two|more}", n = 50)

plu::ral("{one|two|three|more}", n = 1)
plu::ral("{one|two|three|more}", n = 2)
plu::ral("{one|two|three|more}", n = 3)
plu::ral("{one|two|three|more}", n = 50)
plu::ral("{one|two|three|more}", n = 0)
plu::ral("{one|two|three|more}", n = 1.5)
