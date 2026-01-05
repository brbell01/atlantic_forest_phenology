// Load the MultiPolygon feature collection
var polygons = ee.FeatureCollection("MY_MULTIPOLYGON_FEATURE_COLLECTION");

// Load the ImageCollection
var images = ee.ImageCollection("MY_IMAGE_COLLECTION");

// Initialize an empty list to store the results
var results = ee.List([]);

// Iterate over the polygons
polygons.map(function(polygon) {
  
  // Define the region for the current polygon
  var region = polygon.geometry();

  // Iterate over the images in the ImageCollection
  images.map(function(image) {
    
    // Extract the median value for the current image and polygon
    var median = image.reduceRegion(ee.Reducer.median(), region, 30);
    
    // Add the median value, polygon ID and image ID to the results list
    results = results.add(ee.Dictionary({
      median: median,
      polygon_id: polygon.id(),
      image_id: image.id()
    }));
  });
});

// Create a table from the results list
var table = ee.FeatureCollection(results.map(function(result) {
  return ee.Feature(null, result);
}));

// Export the table to Google Drive
Export.table.toDrive({
  collection: table,
  description: 'MedianValues',
  folder: 'MY_FOLDER',
  fileNamePrefix: 'MedianValues'
});


