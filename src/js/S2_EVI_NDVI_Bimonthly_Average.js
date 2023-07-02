var MATA_ESCURA_HECTARES = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"),
    MATA_ESCURA_PARK = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    s2Clouds = ee.ImageCollection("COPERNICUS/S2_CLOUD_PROBABILITY"),
    S2 = ee.ImageCollection("COPERNICUS/S2");

print(MATA_ESCURA_HECTARES)

// imports
var helpers = require('users/brbell01/AF_Phenology:Helpers');
var constants = require('users/brbell01/AF_Phenology:Constants');

// Constants
var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots'
var S2_RED_BAND = 'B4';
var S2_NIR_BAND = 'B8';

var START_DATE = constants.STUDY_START_DATE;
var END_DATE = constants.STUDY_END_DATE;
var MAX_CLOUD_PROBABILITY = 60

//// Functions

/// Bi-monthly index
////
//// PROBLEM - NOT ALL MONTHS HAVE IMAGES from 15th - on
////
var generateBiMonthlyIndexChart = function(indexName, imageCollection, featureCollection, regionName) {
  var MONTHS = ee.List.sequence(1, 12);
  var MONTH_NAMES = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'];
  
  // Define a dictionary that associates property names with values and labels.
  var monthInfo = {
    '01': {v: 1, f: 'Jan'},
    '02': {v: 2, f: 'Feb'},
    '03': {v: 3, f: 'Mar'},
    '04': {v: 4, f: 'Apr'},
    '05': {v: 5, f: 'May'},
    '06': {v: 6, f: 'Jun'},
    '07': {v: 7, f: 'Jul'},
    '08': {v: 8, f: 'Aug'},
    '09': {v: 9, f: 'Sep'},
    '10': {v: 10, f: 'Oct'},
    '11': {v: 11, f: 'Nov'},
    '12': {v: 12, f: 'Dec'}
  };
  
  // Organize property information into objects for defining x properties and
  // their tick labels.
  var xPropValDict = {};  // Dictionary to codify x-axis property names as values.
  var xPropLabels = [];   // Holds dictionaries that label codified x-axis values.
  var monthValueIndex = 1
  for (var key in MONTH_NAMES) {
    var monthKey = MONTH_NAMES[key];
    var monthLabelName = monthInfo[monthKey].f;
    var suffix = '_' + indexName;
    var indexNameKey1 = monthKey + '-01' + suffix
    var indexNameKey2 = monthKey + '-15' + suffix
    
    xPropValDict[indexNameKey1] = monthValueIndex;
    
    xPropLabels.push({v: monthValueIndex, f: monthLabelName + '-01'});
    monthValueIndex++;
    
    xPropValDict[indexNameKey2] = monthValueIndex;
    xPropLabels.push({v: monthValueIndex, f: monthLabelName + '-15'});
    monthValueIndex++;
  }
  
  MONTH_NAMES = ee.List(MONTH_NAMES);
  

  var HECTIDNAMES = MATA_ESCURA_HECTARES.aggregate_array('Hectare_ID')
  var HECTIDkeys = ee.List.sequence(1, ee.Number(MATA_ESCURA_HECTARES.size()))
  
  var xSeriesValDict = {} // Dictionary to codify series property names as values.
  var xSeriesLabels = [] // Holds dictionaries that label codified series values.
  var SeriesValueIndex = 1
  for (var id in HECTIDNAMES) {
    HectIDkey = HECTIDkeys[id]
    HectIDLabel = HECTIDNAMES[id].get('Hectare_ID')
    xSeriesValDict[HectIDkey] = SeriesValueIndex;
    xSeriesLabels.push({v: SeriesValueIndex, f: HectIDLabel});
    SeriesValueIndex++;
  }
  

  // // Function to calculate average index within the given geometry
  function meanIndex(image, geometry) {
    var stats = image.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: geometry,
      scale: 10
    });
    return stats.get(indexName + "_mean");
  }
  // Function to select all images for the given month and compute a mean composite
  function monthIndex(month, imageCollection, geometry) {
    var monthImages = imageCollection.filter(
      // Use calendarRange function to filter by month
      ee.Filter.calendarRange({start: month, field: 'month'})
      );
    var firstOfMonthImages = monthImages.filter(
      // Use calendarRange function to filter by month
      ee.Filter.calendarRange({start: 1, end: 15, field: 'day_of_month'})
      );
      
    var endOfMonthImages = monthImages.filter(
      // Use calendarRange function to filter by month
      ee.Filter.calendarRange({start: 15, end: 31, field: 'day_of_month'})
      );
    var meanImage_1 = firstOfMonthImages.reduce(ee.Reducer.mean());
    var mean_1 = meanIndex(meanImage_1, geometry);
    var meanImage_2 = endOfMonthImages.reduce(ee.Reducer.mean());
    var mean_2 = meanIndex(meanImage_2, geometry);
    return ee.List([ee.Number(mean_1), ee.Number(mean_2)]);
  }
  
  //function to get the average of a band over a month in a supplied region
  function getIndexByMonth(feature, imageCollection) {
    // filter by bounds
    var filtered = imageCollection.filterBounds(feature.geometry());
    
    // Map the function over the list
    var indecesOfMonths = MONTHS.map(function(month) { return monthIndex(month, filtered, feature.geometry()); }).flatten();
    var IndexMonthNames = MONTH_NAMES.map(function (name) {
      name = ee.String(name);
      var first = name.cat('-01_').cat(indexName);
      var second = name.cat('-15_').cat(indexName);
      return [first, second];
    }).flatten();
    
    var IndexByMonth = ee.Dictionary.fromLists(IndexMonthNames, indecesOfMonths);
    var properties = feature.toDictionary().combine(IndexByMonth);
    return ee.Feature(feature.geometry(), properties);
  }
  
  var meanIndeces = featureCollection.map(function(feature) { return getIndexByMonth(feature, imageCollection); })
  print(meanIndeces);
  // Define the chart and print it to the console.
  var combinedIndexChart = ui.Chart.feature
                  .byProperty({
                    features: meanIndeces,
                    xProperties: xPropValDict,
                    seriesProperty: 'Hectare_ID'
                  })
                  .setChartType('ColumnChart')
                  .setOptions({
                    title: 'Average BiMonthly ' + indexName + ' (' + AREA_NAME + ')',
                    hAxis: {
                      title: 'Month',
                      titleTextStyle: {italic: false, bold: true},
                      ticks: xPropLabels
                    },
                    vAxis: {
                      title: indexName,
                      titleTextStyle: {italic: false, bold: true}
                    },
                    colors: ['604791', '1d6b99', '39a8a7', '0f8755', '76b349', 'f0af07', 'e37d05', 'cf513e', '96356f', '724173'],
                  }).setSeriesNames;
  return combinedIndexChart;
}


//////////////////////////////////////////////////////////////////////////
//////////
//Main
/////////
//////////////////////////////////////////////////////////////////////////

// Filter by study dates and park bounds
var filter = ee.Filter.and(
    ee.Filter.bounds(MATA_ESCURA_PARK.geometry()), 
    ee.Filter.date(START_DATE, END_DATE));
    
S2 = S2.filter(filter)
    .map(helpers.S2AddIndeces);

// Join S2 SR with cloud probability dataset to add cloud mask.
var s2WithCloudProperty = helpers.s2AddCloudProbability(S2, s2Clouds, filter);
var s2CloudFiltered = s2WithCloudProperty.filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', MAX_CLOUD_PROBABILITY);
var s2CloudMasked = helpers.s2MaskClouds(s2WithCloudProperty, MAX_CLOUD_PROBABILITY);

print(generateBiMonthlyIndexChart(constants.EVI_NAME, s2CloudMasked, MATA_ESCURA_HECTARES, AREA_NAME));
print(generateBiMonthlyIndexChart(constants.NDVI_NAME, s2CloudMasked, MATA_ESCURA_HECTARES, AREA_NAME));