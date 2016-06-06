#' Display States of the Federal Republic of Nigeria
#' 
#' 
#' @param gpz Geopolitical zone
#' @return The States of Nigeria as a whole or by zones
#' @examples
#' states()
#' states(2)
#' states("se")
states <- function(gpz = NULL)
  {
  s <- c("Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa",
         "Benue", "Borno", "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti",
         "Enugu", "Gombe", "Imo", "Jigawa", "Kaduna", "Kano", "Katsina",
         "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa", "Niger", "Ogun",
         "Ondo", "Osun", "Oyo", "Plateau", "Rivers", "Sokoto", "Taraba",
         "Yobe", "Zamfara")
  if (!is.null(gpz))
    switch (gpz,
      "nc" = s <- s[c(7, 22, 23, 25, 26, 31)],
      "ne" = s <- s[c(2, 5, 8, 15, 34, 35)],
      "nw" = s <- s[c(17:21, 33, 36)],
      "se" = s <- s[c(1, 4, 11, 14, 16)],
      "ss" = s <- s[c(3, 6, 9, 10, 12, 32)],
      "sw" = s <- s[c(13, 24, 27:30)]
    )
  print(s)
}