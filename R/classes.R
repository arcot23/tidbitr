setClass("NumWithComma")
setAs("character", "NumWithComma", function(from)
  as.numeric(gsub("[,$% ]", "", from)))

setClass("OraDate")
setAs("Date", "OraDate", function(from)
  as.Date(x, "%d-%b-%y"))
