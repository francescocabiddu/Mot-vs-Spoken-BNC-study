round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

notify_me <- function(subj = "R: Come back to me", pass, recipient) {
  send.mail(from = "fcabiddur@gmail.com",
            to = recipient,
            subject= subj,
            body = "DO NOT REPLY",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name="fcabiddur@gmail.com", passwd=pass, ssl=TRUE),
            authenticate = TRUE,
            send = TRUE)
}
