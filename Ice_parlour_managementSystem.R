ice_cream <- vector("list", 100)
quantity <- integer(100)
num_ice_cream <- 0
data_file <- "ice_cream_data.txt"

load_data <- function() {
  if (file.exists(data_file)) {
    data <- read.table(data_file, header = TRUE, stringsAsFactors = FALSE)
    num_ice_cream <<- nrow(data)
    ice_cream <<- data$IceCream
    quantity <<- data$Quantity
    cat("Data loaded successfully.\n")
  } else {
    cat("No data file found. Starting with empty data.\n")
  }
}

save_data <- function() {
  data <- data.frame(IceCream = ice_cream[1:num_ice_cream], Quantity = quantity[1:num_ice_cream])
  write.table(data, file = data_file, sep = "\t", row.names = FALSE)
  cat("Data saved successfully.\n")
}

display_ice_cream <- function() {
  cat("\n\t---------- ICE CREAM DETAILS ----------\n")
  if (num_ice_cream == 0) {
    cat("\t\tNo Ice Cream to display.\n")
  } else {
    cat("Ice Cream Name\t\t\tQuantity\n")
    for (i in 1:num_ice_cream) {
      cat(sprintf("%s\t\t\t%d\n", ice_cream[i], quantity[i]))
    }
  }
}

add_ice_cream <- function() {
  cat("\n\t---------- ADD NEW ICE CREAM ----------\n")
  if (num_ice_cream == 100) {
    cat("Can not add more Ice Cream.\n")
  } else {
    cat("\tEnter Ice Cream name: ")
    ice_cream[num_ice_cream + 1] <- readline()
    cat("\tEnter Ice Cream quantity: ")
    quantity[num_ice_cream + 1] <- as.integer(readline())
    num_ice_cream <<- num_ice_cream + 1
    cat("\tIce Cream added successfully.\n")
    save_data()
  }
}

delete_ice_cream <- function() {
  cat("\n\t---------- DELETE ICE CREAM DETAILS-----------\n")
  if (num_ice_cream == 0) {
    cat("\t\tNo Ice Cream to delete.\n")
    return()
  }
  cat("\tEnter Ice Cream name to delete: ")
  ice_cream_name <- readline()
  index <- match(ice_cream_name, ice_cream, nomatch = 0)
  if (index == 0) {
    cat("\t\tIce Cream not found.\n")
  } else {
    ice_cream <<- ice_cream[-index]
    quantity <<- quantity[-index]
    num_ice_cream <<- num_ice_cream - 1
    cat("\t\tIce Cream deleted successfully.\n")
    save_data()
  }
}

search_ice_cream <- function() {
  cat("\n\t---------- SEARCH ICE CREAM -----------\n")
  if (num_ice_cream == 0) {
    cat("\t\tNo Ice Cream to search.\n")
    return()
  }
  cat("\tEnter Ice Cream name to search: ")
  ice_cream_name <- readline()
  index <- match(ice_cream_name, ice_cream, nomatch = 0)
  if (index == 0) {
    cat("\t\tIce Cream not found.\n")
  } else {
    cat("\t\tIce Cream Found\n")
    cat("Ice Cream Name\tQuantity\n")
    cat(sprintf("%s\t\t%d\n", ice_cream[index], quantity[index]))
  }
}

update_ice_cream <- function() {
  cat("\n\t---------- UPDATE ICE CREAM -----------\n")
  if (num_ice_cream == 0) {
    cat("\t\tNo Ice Cream to update.\n")
    return()
  }
  cat("\tEnter Ice Cream name to update: ")
  ice_cream_name <- readline()
  index <- match(ice_cream_name, ice_cream, nomatch = 0)
  if (index == 0) {
    cat("\t\tIce Cream not found.\n")
  } else {
    cat("Enter new Ice Cream name: ")
    ice_cream[index] <- readline()
    cat("Enter new quantity: ")
    quantity[index] <- as.integer(readline())
    cat("\t\tIce Cream updated successfully.\n")
    save_data()
  }
}

main <- function() {
  load_data()
  choice <- 0
  repeat {
    cat("\n******* ICE CREAM PARLOUR MANAGEMENT SYSTEM *******\n\n")
    cat("\t1. Display Ice Cream Detail\n")
    cat("\t2. Add New Ice Cream\n")
    cat("\t3. Delete Ice Cream\n")
    cat("\t4. Search Ice Cream\n")
    cat("\t5. Update Ice Cream\n")
    cat("\t6. Exit\n")
    cat("\n\tEnter your choice (1 to 6): ")
    choice <- as.integer(readline())
    switch(choice,
           "1" = display_ice_cream(),
           "2" = add_ice_cream(),
           "3" = delete_ice_cream(),
           "4" = search_ice_cream(),
           "5" = update_ice_cream(),
           "6" = {cat("\n............ Hope You Enjoy .............\n\n")
             break},
           cat("Invalid choice!\n")
    )
  }
}

main()

