//Import helper scripts

var wrapper = require('users/brbell01/AF_Phenology:Sentinel-1/Wrapper');
var helper = require('users/brbell01/AF_Phenology:Sentinel-1/Utilities');
var helpers = require('users/brbell01/AF_Phenology:Helpers');
var constants = require('users/brbell01/AF_Phenology:Constants');

//Import study area and sampling plots

var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots',
    Hectares = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"),
    Park = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    geometry = ee.Feature(Hectares.first()).geometry();

// Define Sentinel-1 Parameters

var parameter = {//1. Data Selection
    START_DATE: "2020-01-01",
    STOP_DATE: "2022-01-01",
    POLARIZATION:'VVVH',
    ORBIT: 'DESCENDING',
    ORBIT_NUMBER: 82 //155,
    GEOMETRY: geometry,
    APPLY_ADDITIONAL_BORDER_NOISE_CORRECTION: true,
    //3.Speckle filter
    APPLY_SPECKLE_FILTERING: true,
    SPECKLE_FILTER_FRAMEWORK: 'MULTI',
    SPECKLE_FILTER: 'LEE SIGMA',
    SPECKLE_FILTER_KERNEL_SIZE: 15,
    SPECKLE_FILTER_NR_OF_IMAGES: 5,
    //4. Radiometric terrain normalization
    APPLY_TERRAIN_FLATTENING: true,
    DEM: ee.Image('USGS/SRTMGL1_003'),
    TERRAIN_FLATTENING_MODEL: 'VOLUME',
    TERRAIN_FLATTENING_ADDITIONAL_LAYOVER_SHADOW_BUFFER: 100,
    //5. Output
    FORMAT : 'DB',
    CLIP_TO_ROI: false,
    SAVE_ASSETS: false,
    //6. Visualization
    VIS_BAND_RATIO: 'VHVV_ratio' // or 'RVI'
}

//Preprocess the S1 collection
var s1_preprocess = wrapper.s1_preproc(parameter);

var s1 = s1_preprocess[0];
s1_preprocess = s1_preprocess[1];

//Filter to consistent orbit number
s1 = s1.filter(ee.Filter.eq('relativeOrbitNumber_start', 
    parameter.ORBIT_NUMBER));

print(s1,  "S1 collection - single orbit")

s1_preprocess = s1_preprocess.filter(ee.Filter.eq('relativeOrbitNumber_start', 
                parameter.ORBIT_NUMBER));

print(s1_preprocess,  "ARD processed S1 collection - single orbit")

//Add ratio band   

if (parameter.POLARIZATION=='VVVH'){
    if (parameter.FORMAT=='DB'){
   var s1_preprocess_view = s1_preprocess.map(helper.add_ratio_lin).map(helper.lin_to_db2);
   var s1_view = s1.map(helper.add_ratio_lin).map(helper.lin_to_db2);
   }
   else {
   var s1_preprocess_view = s1_preprocess.map(helper.add_ratio_lin);
   var s1_view = s1.map(helper.add_ratio_lin);
   }
}

print(s1_preprocess_view, "S1 ARD collection with Band Ratio");

//// Functions

var generateIndexChart = function(indexName, imageCollection, featureCollection, regionName) {

    // // Function to calculate average index within the given geometry
    function medianIndex(image, geometry) {
      var stats = image.reduceRegion({
        reducer: ee.Reducer.median(),
        geometry: geometry,
        scale: 10
      });
      
      return stats.get(indexName);
    }
  
    //function to get a list of band values and respective dates in supplied regions
    function getAverageIndex(feature, imageCollection) {
      // filter by bounds
      var filtered = imageCollection//.filterBounds(feature.geometry());
      
      var averages = filtered.toList(1000).map(function (image) {
        image = ee.Image(image)
        var median = medianIndex(image, feature.geometry());
        var date = image.date()
        return ee.List([date.format('y-M-d'), median])
      })
      //.filter(ee.Filter.listContains('item', null).not())
      
      
      var propertyKey = indexName
      return feature.set(propertyKey, averages)
    }
    
    var meanIndeces = featureCollection.map(function(feature) { return getAverageIndex(feature, imageCollection); })
    return meanIndeces
  }
  
var hectaresImageIndices = generateIndexChart(constants.VV_NAME, s1_preprocess_view, Hectares, AREA_NAME);
  hectaresImageIndices = generateIndexChart(constants.VH_NAME, s1_preprocess_view, hectaresImageIndices, AREA_NAME);
  hectaresImageIndices = generateIndexChart(constants.VHVVratio_NAME, s1_preprocess_view, hectaresImageIndices, AREA_NAME);

print(hectaresImageIndices,  "Median Backscatter from 1-Ha Plots")

//var data = hectaresImageIndices.toArray()

// hectdata = ee.FeatureCollection(data
//                         .map(function(element){
//                         return ee.Feature(null,{prop:element})}))


// print(hectdata)

// //Export.table.toDrive(ee.Element(chartArray));
// Export.table.toDrive({
//   collection: hectdata,
//   folder: 'earthengine',
//   description:'test_chartArray',
//   fileFormat: 'CSV'
// });