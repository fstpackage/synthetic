
# define generators used in tests

# sparse integers
sparse_ints <- synthetic_table(
  Integers = template_integer(1, 10),
  id = "integer sparse"
)

# random integers
random_ints <- synthetic_table(
  Integers = template_integer(1, nr_of_rows()),
  id = "integer random"
)
