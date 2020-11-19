update_supersecrets <- function(type = c("add", "rm")) {

  type <- match.arg(type)

  if (type == "add") {
    message("Adding new user...")
  } else {
    message("Removing existing user...")
  }


  usr <- readline("Please, type the username: ")
  user_in <- usr %in% super_secret()[["username"]]

  if (type == "add") {
    stopifnot(`Username already present.` = sum(user_in) == 0L)
  } else {
    stopifnot(`User not in the database` = any(user_in))
  }



  psw <- getPass::getPass("Please, type their (non-empty) password: ",
                          noblank = TRUE)


  on.exit(Sys.chmod(super_secret(TRUE), mode = "0400"))
  Sys.chmod(super_secret(TRUE), mode = "0600")

  if (type == "add") {
    role <- readline("Set privileges:
      u := ubep,
      v := tip-v,
      a := agenas,
      b := basic
    ")
    stopifnot(`Role must be "a", "b", "u", "v"` =
                role %in% c("a", "b", "u", "v"))

    role <- switch(role,
        u = "ubep",
        v = "tip-v",
        a = "agenas",
        b = "basic")

    readr::write_lines(
      paste(usr, sodium::password_store(psw), role, sep = ", "),
      super_secret(TRUE),
      append = TRUE
    )

    message("User added")
    return(invisible(usr))
  }


  hash <- dplyr::filter(super_secret(), .data$username == usr)[["password"]]
  stopifnot(`Password does not match` =
              sodium::password_verify(hash, psw))

  dplyr::filter(super_secret(), .data$username != usr) %>%
    readr::write_csv(super_secret(TRUE))

  message("User removed.")
  return(invisible(usr))
}



