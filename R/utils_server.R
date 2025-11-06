# server_upload <- function() {
#   selection <- utils::menu(c("Only tarball", "Only renv.lock", "Both"), title = "Select upload option")
#   selection <- as.character(selection)
#   switch(selection,
#          "1" = {
#           cmd <- "sftp -i ~/.ssh/id_rsa beitner@tiger.uni-frankfurt.de"
# 
#          },
#          "2" = {
#           cmd <- "sftp -i ~/.ssh/id_rsa beitner@tiger.uni-frankfurt.de"
# 
#          },
#          "3" = {
#           cmd <- "sftp -i ~/.ssh/id_rsa beitner@tiger.uni-frankfurt.de"
# 
#          },
#          stop("Invalid selection")
#   )
# 
#   system(cmd)
# }

