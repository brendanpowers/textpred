profanity <- function () {
  #Returns an regular expression of some english profanity
  
# The short list  
# Thank you George Carlin for publisizing this list from George Carlin
bannedwords <-c("shit", "piss", "fuck", "cunt", "cocksucker"," motherfucker", "tit")
"\\bshit\\w+|\\bpiss\\w+|\\bfuck\\w+|\\bcunt\\w+|\\bcocksuck\\w+|tits|titty|tit|titties"
}