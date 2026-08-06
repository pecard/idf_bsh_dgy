check_username <- function() {
  answer <- svDialogs::dlgInput("Please enter your name", "")$res
  if(nchar(answer) <= 3) return(F)
  return(answer)
}