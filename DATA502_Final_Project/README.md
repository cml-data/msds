## UFO Anomaly Detector App

Uses the Kaggle UFO dataset to display data in a shiny app with a leaflet map to show sitings in the US.
Shows a couple of different visuals determined by date and location.

The idea behind this app is that most UFO sightings are bogus pieces of data and that if you really want
to find a UFO, you want to look for anamalous things: sighting during the day, seeing a shape out of the ordinary,
many people seeing the same thing.  So, it lets you look in an area and see if there are sightings that match
an anomalous state.

To run the app:

Rscript -e 'library(methods); shiny::runApp("path-to-project/", launch.browser = TRUE)'
