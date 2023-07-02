//Import helper scripts

var wrapper = require('users/brbell01/AF_Phenology:Sentinel-1/Wrapper');
var helper = require('users/brbell01/AF_Phenology:Sentinel-1/Utilities');
var helpers = require('users/brbell01/AF_Phenology:Helpers');
var constants = require('users/brbell01/AF_Phenology:Constants');

//Import study area and sampling plots

var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots',
    Park = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    Hectares = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"),
    geometry = ee.Feature(Park.first()).geometry();


// Define Sentinel-1 Parameters
var parameter = {//1. Data Selection
    START_DATE: "2016-01-01",
    STOP_DATE: "2022-01-01",
    POLARIZATION:'VVVH',
    ORBIT: 'DESCENDING',
    ORBIT_NUMBER: 82,  //155
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
print(s1_preprocess_view, "S1 ARD collection with Indices");

//Create a list of images to sample across a year
var JanImage = s1_preprocess_view.filter(ee.Filter.date('2018-01-01', '2018-02-01')).first()
var AprilImage = s1_preprocess_view.filter(ee.Filter.date('2018-04-01', '2018-05-01')).first()
var JulyImage = s1_preprocess_view.filter(ee.Filter.date('2018-07-01', '2018-08-01')).first()
var OctoberImage = s1_preprocess_view.filter(ee.Filter.date('2018-10-01', '2018-11-01')).first()

var SampleImages = ee.ImageCollection([JanImage, AprilImage, JulyImage, OctoberImage])

//Visualize
//visualization parameters 
var visparam = {bands:'angle', min: 40, max: 50, Opacity: 0.25}

Map.addLayer(JanImage.select('angle'), visparam, 'Jan 2018 processed image angle')


// //Just to prove the ARD processing does change angle properties
// var JanImage_1 = s1.filter(ee.Filter.date('2018-01-01', '2018-02-01')).first()
// Map.addLayer(JanImage_1.select('angle'), visparam, 'Jan 2018 unprocessed image angle')

// Add ROI
var empty = ee.Image().byte();
var parkboundary = empty.paint({featureCollection: Park, color: 1, width: 3});
var ha_boundary = empty.paint({featureCollection: Hectares, color: 1, width: 3});
Map.addLayer(parkboundary, {palette: '00FF00'}, 'Park');
Map.addLayer(ha_boundary, {palette: '0000FF'}, '1-Ha Plots');

//Set the aoi for sampling
var aoi = Park
var sample = SampleImages.first().sample({
    region: aoi,
    scale: 30,
    tileScale: 16,
    numPixels: 3000 //Note that numPixels is the approximated number of samples to collect
});

//Scatterchart 
var chart1 = ui.Chart.feature.byFeature(sample, 'angle', ['VH'])
              .setChartType('ScatterChart')
              .setOptions({pointSize: 2,
                           pointColor: 'green',
                           title: AREA_NAME,
                           titleY: 'Sentinel-1 VH (db)',
                           titleX: 'angle',
                           trendlines:{0: {
                                          type: 'linear',
                                          color: 'black',
                                          showR2: true,
                                          visibleInLegend: true}}});
print(chart1);
