/* Helper functions. 
 *
 * Inspired by https://medium.com/google-earth/making-it-easier-to-reuse-code-with-earth-engine-script-modules-2e93f49abb13
 */

var constants = require('users/brbell01/AF_Phenology:Constants');

/* adds a mask to areas covered by clouds.
 *
 * image - the image to add the cloud mask.
 * returns - an image with the cloud mask.
 */
exports.s2_level1c_cloudmask = function (image) {
  var qa = image.select('QA60');
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
             qa.bitwiseAnd(cirrusBitMask).eq(0));
  return image.updateMask(mask)
      .select("B.*")
      .copyProperties(image, ["system:time_start"]);
}

exports.s2AddCloudProbability = function (s2, s2Clouds, filter) {
  
  s2 = s2.filter(filter);
      
  s2Clouds = s2Clouds.filter(filter);
  
  // Join S2 SR with cloud probability dataset to add cloud mask.
  var s2WithCloudProperty = ee.Join.saveFirst('cloud_mask').apply({
    primary: s2,
    secondary: s2Clouds,
    condition:
        ee.Filter.equals({leftField: 'system:index', rightField: 'system:index'})
  });
  return ee.ImageCollection(s2WithCloudProperty);
}


exports.s2MaskClouds = function (s2WithCloudProbability, maxCloudProbability) {
  function maskClouds(img) {
    var clouds = ee.Image(img.get('cloud_mask')).select('probability');
    var isNotCloud = clouds.lt(maxCloudProbability);
    return img.updateMask(isNotCloud);
  }
  
  // The masks for the 10m bands sometimes do not exclude bad data at
  // scene edges, so we apply masks from the 20m and 60m bands as well.
  // Example asset that needs this operation:
  // COPERNICUS/S2_CLOUD_PROBABILITY/20190301T000239_20190301T000238_T55GDP
  function maskEdges(s2_img) {
    return s2_img.updateMask(
        s2_img.select('B8A').mask().updateMask(s2_img.select('B9').mask()));
  }
  
  s2WithCloudProbability = s2WithCloudProbability.map(maskEdges);
  
  var s2CloudMasked = s2WithCloudProbability.map(maskClouds);
      
  return s2CloudMasked;
}
/* Calculates and adds an ndvi band to a S2 multi-band image.
 *
 * image - the image to add the ndvi band to.
 * returns - the image with an ndvi band added.
 */
exports.S2addNDVI = function (image) {
  var ndvi = image.normalizedDifference([constants.S2_NIR_BAND, constants.S2_RED_BAND]).rename(constants.NDVI_NAME);
  return image.addBands(ndvi);
}

exports.S2addEVI = function (image) {
  var EVI = image.expression(
      '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'NIR' : image.select('B8').divide(10000),
      'RED' : image.select('B4').divide(10000),
      'BLUE': image.select('B2').divide(10000)}).rename(constants.EVI_NAME);
  return image.addBands(EVI);
};

exports.S2AddIndeces = function (image) {
  return exports.S2addNDVI(exports.S2addEVI(image));
}


/* Returns random entries of a feature collection corresponding to the provided percentage.
 * collection - feature collection from which to generate the random samples.
 * percentage - the percentage of the original to return.
 * returs - a feature collection of random entries corresponding to an amount equal to the procided percentage.
 */
exports.randomFeaturesByPercentage = function (collection, percentage) {
  var fc = collection.randomColumn()
  return fc.filter('random < ' + percentage.toString())
}

exports.randomFeaturesByCount = function (collection, numberOfFeatures) {
  var totalFeatures = collection.size().getInfo();
  var requiredPercentage = (numberOfFeatures/totalFeatures).toFixed(10);

  var randomFeatures = exports.randomFeaturesByPercentage(collection, requiredPercentage);

  return randomFeatures;
}

exports.featuresWithMostImages = function (featureCollection, imageCollection, sampleCount, desiredFeatureCount) {
  var imgNumKey = "num_imgs";
  return exports.randomFeaturesByCount(featureCollection, sampleCount)
  .map( function (feature) {
    return feature.set(imgNumKey, imageCollection.filterBounds(feature.geometry()).size());
  })
  .sort(imgNumKey, false)
  .limit(desiredFeatureCount);
}


// Adopted from 
// https://developers.google.com/earth-engine/tutorials/community/modis-ndvi-time-series-animation
//
exports.animatedIndexURL = function (indexName, imageCollection, region, startDate, endDate) {
  var col = imageCollection.map(function(img) {
    var doy = ee.Date(img.get('system:time_start')).getRelative('day', 'year');
    return img.set('doy', doy);
  });
  
  // Make a day-of-year sequence from 1 to 365 with a 16-day step.
  var doyList = ee.List.sequence(1, 365);

  // Import the collection.
  var indexCol = col.select(indexName);
  var distinctDOY = indexCol.filterDate(startDate, endDate);
  // Define a filter that identifies which images from the complete collection
  // match the DOY from the distinct DOY collection.
  var filter = ee.Filter.equals({leftField: 'doy', rightField: 'doy'});
  
  // Define a join.
  var join = ee.Join.saveAll('doy_matches');
  
  // Apply the join and convert the resulting FeatureCollection to an
  // ImageCollection.
  var joinCol = ee.ImageCollection(join.apply(distinctDOY, indexCol, filter));
  // Apply median reduction among matching DOY collections.
  var comp = joinCol.map(function(img) {
    var doyCol = ee.ImageCollection.fromImages(
      img.get('doy_matches')
    );
    return doyCol.reduce(ee.Reducer.median());
  });
  
  // Animation setup
  // Define RGB visualization parameters.
  var visParams = {
    min: -1.0,
    max: 1.0,
    palette: [
      'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
      '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
      '012E01', '011D01', '011301'
    ],
  };
  
  // Create RGB visualization images for use as animation frames.
  var rgbVis = comp.map(function(img) {
    return img.visualize(visParams).clip(region);
  });
  
  // Define GIF visualization parameters.
  var gifParams = {
    'region': region.geometry(),
    'dimensions': 200,
    'crs': 'EPSG:3857',
    'framesPerSecond': 30
  };
  
  return rgbVis.getVideoThumbURL(gifParams)
}

exports.animatedImageURL = function (indexName, imageCollection, region, startDate, endDate) {
  var col = imageCollection.map(function(img) {
    var doy = ee.Date(img.get('system:time_start')).getRelative('day', 'year');
    return img.set('doy', doy);
  });
  
  // Make a day-of-year sequence from 1 to 365 with a 16-day step.
  var doyList = ee.List.sequence(1, 365);

  // Import the collection.
  var indexCol = col.select(indexName);
  var distinctDOY = indexCol.filterDate(startDate, endDate);
  // Define a filter that identifies which images from the complete collection
  // match the DOY from the distinct DOY collection.
  var filter = ee.Filter.equals({leftField: 'doy', rightField: 'doy'});
  
  // Define a join.
  var join = ee.Join.saveAll('doy_matches');
  
  // Apply the join and convert the resulting FeatureCollection to an
  // ImageCollection.
  var joinCol = ee.ImageCollection(join.apply(distinctDOY, indexCol, filter));
  // Apply median reduction among matching DOY collections.
  var comp = joinCol.map(function(img) {
    var doyCol = ee.ImageCollection.fromImages(
      img.get('doy_matches')
    );
    return doyCol.reduce(ee.Reducer.median());
  });
  
  // Animation setup
  // Define RGB visualization parameters.
  
 var visParams = {
  bands: ['B4_median', 'B3_median', 'B2_median'],
  min: 0,
  max: 3000,
};

  
  // Create RGB visualization images for use as animation frames.
  var rgbVis = comp.map(function(img) {
    return img.visualize(visParams).clip(region);
  });
  
  // Define GIF visualization parameters.
  var gifParams = {
    'region': region.geometry(),
    'dimensions': 250,
    'framesPerSecond': 10
  };
  
  return rgbVis.getVideoThumbURL(gifParams)
}


exports.numberOfImagesPerSeason = function (imageCollection, season_names, season_start_indeces,startDate, endDate) {
  // create start index for each season
  
  var startIndexDict = ee.Dictionary.fromLists(season_names, season_start_indeces);
  
   var years = ee.List.sequence(ee.Date(startDate).get('year'), ee.Date(endDate).get('year'));
   
    var seasonFunction = function(season_name) {
    return ee.List([season_name, years.map(function (year) { return annualFunction(year, season_name) }).reduce(ee.Reducer.sum())]);
  }
  
  var annualFunction = function  (year, season_name) {
    var startDate = ee.Date.fromYMD(year, startIndexDict.get(season_name), 1);
    var endDate = startDate.advance(3, "month");
    var dateRange = ee.DateRange(startDate, endDate);
    
    return imageCollection.filterDate(dateRange).size();
  }
   
  return season_names.map(seasonFunction)
}

exports.addDateTaken = function(image) {
  // Get the timestamp and convert it to a date.
var date = ee.Date(image.get('system:time_start'));
return image.set('readable_date', date);
};

