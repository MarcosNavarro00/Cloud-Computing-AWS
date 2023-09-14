#!/bin/bash
exec > >(sudo tee /var/log/user-data.log|sudo logger -t user-data -s 2>sudo /dev/console) 2>&1
sudo yum -y update
sudo amazon-linux-extras install -y epel
sudo yum install -y R
sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
sudo yum install -y gdebi-core
sudo yum install -y libcurl-devel
sudo yum install -y libxml2-devel
sudo yum install -y openssl-devel
sudo yum install -y postgresql-devel
sudo su - -c "R -e \"install.packages(c('tidyverse', 'jsonlite', 'shinythemes', 'dplyr', 'png', 'ggplot2', 'ggjoy', 'DBI', 'RPostgres', 'paws.management', 'shinyjs'), repos='https://cran.rstudio.com/')\""
wget https://raw.githubusercontent.com/JacekCzupyt/CloudComputingProject/master/app.R
wget https://raw.githubusercontent.com/JacekCzupyt/CloudComputingProject/master/R/utils.R
wget https://github.com/JacekCzupyt/CloudComputingProject/raw/master/clickme.png
wget https://download3.rstudio.org/centos7/x86_64/shiny-server-1.5.18.987-x86_64.rpm
sudo yum install -y --nogpgcheck shiny-server-1.5.18.987-x86_64.rpm
sudo rm -r /srv/shiny-server/*
sudo mv clickme.png /srv/shiny-server
sudo mv app.R /srv/shiny-server
sudo mkdir /srv/shiny-server/R
sudo mv utils.R /srv/shiny-server/R