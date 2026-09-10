check_username <- function() {
  answer <- svDialogs::dlgInput("Please enter your name", "Paulo E. Cardoso; Sandra Rodrigues")$res
  if(nchar(answer) <= 3) return(F)
  return(answer)
}