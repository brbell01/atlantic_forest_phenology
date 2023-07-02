//Import helper scripts

var wrapper = require('users/brbell01/AF_Phenology:Sentinel-1/Wrapper');
var helper = require('users/brbell01/AF_Phenology:Sentinel-1/Utilities');
var helpers = require('users/brbell01/AF_Phenology:Helpers');
var constants = require('users/brbell01/AF_Phenology:Constants');

//Import study area and sampling plots

var AREA_NAME = 'REBIO Mata Escura 1-Ha Plots',
    //Hectares = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura_Hectares_new"),
    Hectare1 = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura/mataescura_area_2"),
    Hectare2 = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura/mataescura_area_6"),
    Hectare3 = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura/mataescura_area_11"),
    Hectare4 = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura/mataescura_area_12"),
    Hectare5 = ee.FeatureCollection("projects/af-phenology/assets/Hectares/1_MataEscura/mataescura_area_13"),
    Park = ee.FeatureCollection("projects/af-phenology/assets/Parks/REBIO_Mata_Escura"),
    geometry = ee.Feature(Hectare1.first()).geometry();


// Define Sentinel-1 Parameters
var parameter = {//1. Data Selection
    START_DATE: "2019-01-01",
    STOP_DATE: "2022-01-01",
    POLARIZATION:'VVVH',
    ORBIT: 'DESCENDING',
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

//Filter to consistent orbit number, first acquisition orbit
//s1 = s1.filter(ee.Filter.eq('relativeOrbitNumber_start', 
//s1.first().getNumber('relativeOrbitNumber_start')));
s1_preprocess = s1_preprocess.filter(ee.Filter.eq('relativeOrbitNumber_start', 
                s1_preprocess.first().getNumber('relativeOrbitNumber_start')));
print(s1_preprocess.first().getNumber('relativeOrbitNumber_start'),  "ARD processed S1 collection - single orbit = ")

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


//Set the aoi here
var aoi = Hectare2
// var sample = image.sample({region: aoi,
//     scale: 30,
//     tileScale: 16,
//     numPixels: 3000 //Note that numPixels is the approximated number of samples to collect
// });


////Scatterchart 
// var chart2 = ui.Chart.feature.byFeature(sample, 'EVI', ['Sigma0_VV'])
//               .setChartType('ScatterChart')
//               .setOptions({pointSize: 2,
//                            pointColor: 'red',
//                            title: "REBIO Mata Escura",
//                            titleY: 'Sigma0_VV',
//                            titleX: 'EVI (dl)',
//                            trendlines:{0: {
//                                           type: 'linear',
//                                           color: 'green',
//                                           showR2: true,
//                                           visibleInLegend: true}}});
// print(chart2);

// var chart2 = ui.Chart.feature
// .byProperty({
//   features: sample,
//   xProperties: 'system:time_start',
//   seriesProperty: 'Hectare_ID'
// })
// .setChartType('LineChart')
// .setOptions({
//   title: 'REBIO Mata Escura' + indexName + ' (' + AREA_NAME + ' , Plot 2)',
//   hAxis: {
//     title: 'Month',
//     titleTextStyle: {italic: false, bold: true},
//     ticks: xPropLabels
//   },
//   vAxis: {
//     title: indexName,
//     titleTextStyle: {italic: false, bold: true}
//   },
//   colors: ['604791', '1d6b99', '39a8a7', '0f8755', '76b349', 'f0af07', 'e37d05', 'cf513e', '96356f', '724173'],
// });

// print(chart2)


//Also explore by year

var doybByYearChart = 
            ui.Chart.image
            .doySeriesByYear({
            imageCollection: s1_preprocess_view,
            bandName: 'VH',
            region: aoi,
            regionReducer: ee.Reducer.median(),
            scale: 30,
            sameDayReducer: ee.Reducer.median(),
            startDay:1,
            endDay:365,
})

print(doybByYearChart)

