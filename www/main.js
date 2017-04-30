window.onload = function () {
  
  mapboxgl.accessToken = 'pk.eyJ1IjoicGpicm9mIiwiYSI6ImNqMjNvZDBraTAwMjMzMm81MWcxMjA4cjIifQ.-XXQyKK7bZW7Lg4dLJ3Suw';
  var map = new mapboxgl.Map({
      container: 'map',
      style: 'mapbox://styles/mapbox/streets-v9',
      center: [-71.057083, 42.361145],
      zoom: 15.99,
      pitch: 40,
      bearing: 20
  });

  map.on('load', function() {
      map.loadImage('img/logo.png', (error, image) => {
        if (error) throw error;
        map.addImage('tcb', image);
        map.addLayer({
            "id": "points",
            "type": "symbol",
            "source": {
                "type": "geojson",
                "data": {
                    "type": "FeatureCollection",
                    "features": [{
                        "type": "Feature",
                        "geometry": {
                            "type": "Point",
                            "coordinates": [-71.0394, 42.359051]
                        }
                    }]
                }
            },
            "layout": {
                "icon-image": "tcb",
                "icon-size": 0.5
            }
        });
      });
      map.addLayer({
          'id': 'room-extrusion',
          'type': 'fill-extrusion',
          'source': {
              // Geojson Data source used in vector tiles, documented at
              // https://gist.github.com/ryanbaumann/a7d970386ce59d11c16278b90dde094d
              'type': 'geojson',
              'data': 'boston.geojson'
          },
          'paint': {
              // See the Mapbox Style Spec for details on property functions
              // https://www.mapbox.com/mapbox-gl-style-spec/#types-function
              'fill-extrusion-color': {
                  // Get the fill-extrusion-color from the source 'color' property.
                  'property': 'color',
                  'type': 'identity'
              },
              'fill-extrusion-height': {
                  // Get fill-extrusion-height from the source 'height' property.
                  'property': 'height',
                  'type': 'identity'
              },
              'fill-extrusion-base': {
                  // Get fill-extrusion-base from the source 'base_height' property.
                  'property': 'base_height',
                  'type': 'identity'
              },
              // Make extrusions slightly opaque for see through indoor walls.
              'fill-extrusion-opacity': 0.5
          }
      });
  });
  
  // Add zoom and rotation controls to the map.
  map.addControl(new mapboxgl.NavigationControl());
  map.addControl(new mapboxgl.FullscreenControl());
   
   
  function resize(){    
    $(".mapboxgl-canvas").outerHeight($(window).height()-$(".mapboxgl-canvas").offset().top- Math.abs($(".mapboxgl-canvas").outerHeight(true) - $(".mapboxgl-canvas").outerHeight()));
  }
  
  $(window).on("load resize", function(){                      
      resize();
  });
};