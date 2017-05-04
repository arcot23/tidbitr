setClass("NumWithComma")
setAs("character", "NumWithComma", function(from)
  as.numeric(gsub(",", "", from)))
