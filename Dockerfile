FROM rocker/shiny

## install packages from CRAN (and clean up)
RUN Rscript -e "install.packages(c('devtools','reshape2','lubridate','ggplot2','shiny','jsonlite','curl'), repos='https://cran.rstudio.com/')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


COPY ./splitwise.r /srv/shiny-server/splitwise/app.r

## updated config file
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf

#Build
#sudo docker build -t splitwiseshiny1.0 . 

#stop 
#sudo docker stop splitwseshiny 
#rm
#sudo docker rm splitwiseshiny

#run splitwiseshiny1.0
#sudo docker run -d -p 3838:3838 -p 9093:9093 --name splitwiseshiny --rm  splitwiseshiny1.0

#log - replace container id
#docker logs 6484e5da15f595d46da62e77202904bc927be2aed99190abae340271d6ba59c5