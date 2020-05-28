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

later <- c(
  rep("plum", 3), rep("strawberry", 4), rep("orange", 5),
  "chocolate cake", "ice-cream cone", "pickle", "Swiss cheese", "salami",
  "lollipop", "cherry pie", "sausage", "cupcake", "watermelon"
)

paste("The caterpillar ate", plu::ral("{the|both|all of the} apple", mon))
paste("The caterpillar ate", plu::ral("{the|both|all of the} pear", tue))
paste("The caterpillar ate", plu::ral("{the|both|all of the} delicacy", later))

paste("The caterpillar ate", plu::ral("n apple", mon))
paste("The caterpillar ate", plu::ral("n delicacy", later))

paste("The caterpillar ate", plu::ral("n apple", mon, nombre::cardinal))
paste("The caterpillar ate", plu::ral("n delicacy", later, nombre::cardinal))
