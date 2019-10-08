
# define generators used in tests

# sparse integers
sparse_generator <- table_generator(
  "integer sparse",
  function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows, 1, 10)
    )
  }
)

# random integers
random_generator <- table_generator(
  "integer random",
  function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
    )
  }
)
