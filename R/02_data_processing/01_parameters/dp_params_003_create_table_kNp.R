

# create overview table of combinations of parameters k, N and p
# We assign the following information to each combination of parameters k, N and p we use in our analysis:
# `number`: unique identifier for the combination of parameters k, N and p that appears for example in results files

# specify combinations of parameters k, N and p
table_parameters_kNp <- tribble(~number, ~k, ~N, ~p,
                                  "001",  0,  7,  7,
                                  "002",  0,  7, 14,
                                  "003",  0,  7, 21,
                                  "004",  0,  7, 28,
                                  "005",  0,  7, 35,
                                  "006",  7,  7,  7,
                                  "007",  7,  7, 14,
                                  "008",  7,  7, 21,
                                  "009",  7,  7, 28,
                                  "010",  7,  7, 35,
                                  "011", 14,  7,  7,
                                  "012", 14,  7, 14,
                                  "013", 14,  7, 21,
                                  "014", 14,  7, 28,
                                  "015", 14,  7, 35,
                                  "016", 21,  7,  7,
                                  "017", 21,  7, 14,
                                  "018", 21,  7, 21,
                                  "019", 21,  7, 28,
                                  "020", 21,  7, 35,
                                  "021", 28,  7,  7,
                                  "022", 28,  7, 14,
                                  "023", 28,  7, 21,
                                  "024", 28,  7, 28,
                                  "025", 28,  7, 35)


# save `table_parameters_kNp` as csv file
write_csv(x = table_parameters_kNp,
          file = paste0(Directory_Parameters, Path_Table_Parameters_kNp))


