# CDPoRT-RShiny

This is a simple prototype for visualizing CDPoRT Data. It contains a skeleton for some basic data display components, including maps, plots, and tables, as well as upload and download widgets.

The application can be run using [Docker](https://www.docker.com/) and [docker-compose](https://docs.docker.com/compose/). If you already have docker and docker-compose on your system, navigate to the directory that contains this repository and run the command `docker-compose up`. This command will take care of building the image and starting the container. By default, the R-Studio application will listen on port 8852, but this can be changed by updating the port mapping in [docker-compose.yml](docker-compose.yml). The Docker image should already have the required dependencies installed.

If you are not using Docker, you can bring up the application by [running](https://shiny.rstudio.com/reference/shiny/1.7.0/runApp.html) the [app.R](projects/app.R) file. Note that the libraries listed at the top of the file will need to be present on the host machine, as well as any dependencies they may require. 