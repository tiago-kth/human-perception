#* test
#* @get /hi
function() {
  
  'Up and running!'
  
}

#* Saves data
#* @param nat User nationality
#* @param age User age
#* @param mxp User experience with music
#* @param mls User listening habits
#* @param so1 Colors for first song
#* @param so2 Colors for second song
#* @param so3 Colors for thrid song
#* @get /save
function(nat, age, mxp, mls, so1, so2, so3) {
  
  #print(nat)
  
  time <- as.character(Sys.time())
  
  cat(
    paste(time, nat, age, mxp, mls, so1, so2, paste0(so3, '/n'), sep = ";"),
    file="output.txt",
    append=TRUE
  )

}

#* Get data from server
#* @get /get
function(req, res){
  include_file("output.txt", res)
}
