setClass("NumsWithComma")
setAs("character", "NumsWithComma", function(from)
  as.numeric(gsub(",", "", from)))
