# data <- data.frame(
#   "time" = "",
#   "nationality" = "",
#   "age" = "",
#   "music_experience" = "",
#   "music_listening" = "",
#   "song1_colors" = "",
#   "song2_colors" = "",
#   "song3_colors" = ""
# )
# 
# saveRDS(data, file = "data.rds")
# 
# library(plumber)
pr("./api/plumber.R") %>%
  pr_run(port=8000)
# 
# https://github.com/meztez/plumberDeploy
# 
# If you’re just getting started with hosting cloud servers, the DigitalOcean integration included in plumberDeploy will be the best way to get started. You’ll be able to get a server hosting your custom API in just two R commands. Full documentation is available at https://www.rplumber.io/articles/hosting.html#digitalocean-1.
# 
# Create a DigitalOcean account
# Install plumberDeploy. Validate your account with analogsea::account().
# Configure an ssh key for the Digital Ocean account before using methods included in this package. Use analogsea::key_create method or see https://www.digitalocean.com/docs/droplets/how-to/add-ssh-keys/to-account/.
# Run a test command like analogsea::droplets() to confirm that it’s able to connect to your DigitalOcean account.
# Run mydrop <- plumberDeploy::do_provision(). This will start a virtual machine (or “droplet”, as DigitalOcean calls them) and install Plumber and all the necessary prerequisite software. Once the provisioning is complete, you should be able to access port 8000 on your server’s IP and see a response from Plumber.
# Install any R packages on the server that your API requires using analogsea::install_r_package().
# You can use plumberDeploy::do_deploy_api() to deploy or update your own custom APIs to a particular port on your server.
# (Optional) Setup a domain name for your Plumber server so you can use www.myplumberserver.com instead of the server’s IP address.
# (Optional) Configure SSL
# Getting everything connected the first time can be a bit of work, but once you have analogsea connected to your DigitalOcean account, you’re now able to spin up new Plumber servers in DigitalOcean hosting your APIs with just a couple of R commands. You can even write scripts that provision an entire Plumber server with multiple APIs associated.

analogsea::account()

mydrop <- plumberDeploy::do_provision(320705693)
plumberDeploy::do_deploy_api(320705693, path="api", localPath = "/Users/tiago/Documents/GitHub/human-perception/R/api", port=5000, docs = TRUE, overwrite = TRUE)
#plumberDeploy::do_deploy_api(mydrop, path="api", localPath = "/Users/tiago/Documents/GitHub/human-perception/R/api", port=8001, overwrite = TRUE, docs = TRUE)


plumberDeploy::do_remove_api(320705693, path="api", delete = TRUE)

#http://127.0.0.1:8000/save?nat=bra%20hsaha&age=42&mxp=2&mls=1&so1=321&so2=2344&so3=12
