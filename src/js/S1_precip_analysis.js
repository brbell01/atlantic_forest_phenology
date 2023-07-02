

// Several objectives for this script. Verify both ciclical and anomalous rainfall patterns
// Comparing specifically seasonal variation (max and accumulated rainfall over the park) 
//  a typical year and comparing the same season across multiple years.

// imports
var helpers = require('users/brbell01/AF_Phenology:Helpers');
var constants = require('users/brbell01/AF_Phenology:Constants');

var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots',
    Park = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    Hectares = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"), 
    region = ee.Geometry.Polygon(
        [[[-41.4384, -16.0405],[-41.4384, -16.6151],[-40.6309, -16.6151],[-40.6309, -16.0405]]], null, false);

region = Park ; Map.centerObject(region, 12);

var START_DATE = constants.STUDY_START_DATE;
var END_DATE = constants.STUDY_END_DATE;


//import IMERG hourly rainfall data
var rainfall = ee.ImageCollection('NASA/GPM_L3/IMERG_V06');
rainfall = rainfall.filter(ee.Filter.date(START_DATE, END_DATE))

var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots',
    Park = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    Hectares = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"), 
    region = ee.Geometry.Polygon(
        [[[-41.4384, -16.0405],[-41.4384, -16.6151],[-40.6309, -16.6151],[-40.6309, -16.0405]]], null, false);

region = Park

Map.centerObject(region, 12)

//import IMERG rainfall data 
var rainfall = ee.ImageCollection('NASA/GPM_L3/IMERG_V05');

function add_date(img){
    var date  = ee.Date(img.get('system:time_start'));
    var date_daily = date.format('YYYY-MM-dd');
    return img.set('date_daily', date_daily);
}

var startdate = ee.Date.fromYMD(2014,3,1);
var enddate   = ee.Date.fromYMD(2014,4,1);
var rainfall = rainfall
    .filter(ee.Filter.date(START_DATE,END_DATE)).select('precipitationCal')
    .map(add_date);

print(rainfall.limit(3), rainfall.size());

var pkg_trend = require('users/kongdd/public:Math/pkg_trend.js');
var rainfall_daily = pkg_trend.aggregate_prop(rainfall, "date_daily", 'sum');
print(rainfall_daily);

//Map.addLayer(rainfall_daily, {}, 'precp daily');

// // Select the max precipitation and mask out low precipitation values.
// var maxprecipitation = rainfall.select('precipitationCal').max();
// var meanprecipitation = rainfall.select('precipitationCal').mean();
// var mask = maxprecipitation.gt(1);
// maxprecipitation = maxprecipitation.updateMask(mask);
// meanprecipitation = meanprecipitation.updateMask(mask);


// //Plotting data
// //Chart 1: SSR Hourly data
// var chart1 = ui.Chart.image.series(rainfall, region, ee.Reducer.mean(), 1000, 'system:time_start').setOptions({
//           title: 'Surface net solar radiation (Hourly)',
//           vAxis: {title: '[J/m2]'},
// });
// print(chart1);

// //Chart 2: SSR Daily data
// var chart2 = ui.Chart.image.series(rainfall_daily_list, region, ee.Reducer.mean(), 1000, 'system:time_start').setOptions({
//           title: 'Surface net solar radiation (Daily)',
//           vAxis: {title: '[J/m2]'},
// });
// print(chart2);