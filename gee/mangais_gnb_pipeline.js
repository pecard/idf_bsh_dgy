// ************** Mangais da Guiné-Bissau — pipeline parametrizada ****************
// Imports necessários (painel Imports do Code Editor, com estes nomes):
//   mangal, agua, floresta, aberto : FeatureCollection de pontos, propriedade 'landcover' (1..4)
//   roi_l8                         : Polygon (região de interesse para as imagens)
//   area_mangais                   : Table  (cartografia de referência, Area_Mangais_GNB_Rev2)
//   admin1                         : Table  (regiões administrativas, propriedade 'name_1')
//   AP                             : Table  (áreas protegidas, propriedade 'NAME')
//
// Classes: 1 mangal | 2 agua | 3 floresta | 4 aberto

// ============================ CONFIGURAÇÃO ============================
var CONFIG = {
  years: [2025],        // anos a mapear; janela = 1 dez (ano-1) a 15 jan (ano)
  useL9: false,         // juntar Landsat 9 (só anos >= 2022); false reproduz o mapa anterior
  cloudCoverMax: 100,   // filtro CLOUD_COVER (100 = sem filtro, como no script anterior)
  extraIndices: false,  // acrescentar MNDWI e NDWI aos preditores

  trees: 2001,          // árvores do modelo final
  seed: 123,

  // Validação cruzada por blocos espaciais
  cvTrees: 500,         // árvores em cada fold (só para a validação)
  kFolds: 5,
  cvRepeats: 3,         // repetições com atribuição de blocos diferente
  blockDeg: 0.05,       // lado do bloco em graus (~5.5 km)

  classes: [1, 2, 3, 4],
  folder: 'MangaisGEEngine',
  exportImages: true,
  exportTables: true,
  printAreas: false     // imprimir áreas na consola dá timeout (modo interativo); usar o export CSV
};

var PALETTE = [
  'BD4BD5', // 1 mangal
  '04C5FF', // 2 agua
  '866418', // 3 floresta
  'D6D6D6'  // 4 aberto
];

var BASE_BANDS = ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7',
                  'ST_B10', 'NDVI', 'NDBI', 'SAVI', 'EVI'];
var PRED_BANDS = CONFIG.extraIndices ? BASE_BANDS.concat(['MNDWI', 'NDWI']) : BASE_BANDS;

// Nomes das colunas das tabelas de métricas
function keysFor(prefix) {
  return CONFIG.classes.map(function(c) { return prefix + c; });
}
var PA_KEYS = keysFor('pa_');
var UA_KEYS = keysFor('ua_');
var F1_KEYS = keysFor('f1_');
var KM_KEYS = keysFor('km2_');
var CELL_KEYS = [];   // matriz de confusão, linhas = referência, colunas = mapa
CONFIG.classes.forEach(function(r) {
  CONFIG.classes.forEach(function(c) { CELL_KEYS.push('cm_' + r + '_' + c); });
});

// ============================ PRÉ-PROCESSAMENTO ============================
// Escala e mascara imagens Landsat SR (L8/L9).
function prepSrL8(image) {
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);

  var getFactorImg = function(factorNames) {
    var factorList = image.toDictionary().select(factorNames).values();
    return ee.Image.constant(factorList);
  };
  var scaleImg = getFactorImg([
    'REFLECTANCE_MULT_BAND_.|TEMPERATURE_MULT_BAND_ST_B10']);
  var offsetImg = getFactorImg([
    'REFLECTANCE_ADD_BAND_.|TEMPERATURE_ADD_BAND_ST_B10']);
  var scaled = image.select('SR_B.|ST_B10').multiply(scaleImg).add(offsetImg);

  return image.addBands(scaled, null, true)
    .updateMask(qaMask).updateMask(saturationMask);
}

var addIndices = function(image) {
  var ndvi = image.normalizedDifference(['SR_B5', 'SR_B4']).rename(['NDVI']);
  var ndbi = image.normalizedDifference(['SR_B6', 'SR_B5']).rename(['NDBI']);
  var mndwi = image.normalizedDifference(['SR_B3', 'SR_B6']).rename(['MNDWI']);
  var ndwi = image.normalizedDifference(['SR_B3', 'SR_B5']).rename(['NDWI']);
  var savi = image.expression(
    '(1 + l) * ((nir - red) / (nir + red + l))',
    {nir: image.select('SR_B5'), red: image.select('SR_B4'), l: 0.5}).rename('SAVI');
  var evi = image.expression(
    '2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)',
    {red: image.select('SR_B4'), nir: image.select('SR_B5'),
     blue: image.select('SR_B2')}).rename(['EVI']);
  return image.addBands(ndvi).addBands(ndbi).addBands(mndwi)
    .addBands(ndwi).addBands(savi).addBands(evi);
};

function windowFor(year) {
  return {start: (year - 1) + '-12-01', end: year + '-01-15'};
}

function landsatCollection(year) {
  var w = windowFor(year);
  var sources = ['LANDSAT/LC08/C02/T1_L2'];
  var sensors = 'L8';
  if (CONFIG.useL9 && year >= 2022) {
    sources.push('LANDSAT/LC09/C02/T1_L2');
    sensors = 'L8+L9';
  }
  var col = ee.ImageCollection(sources[0]);
  for (var i = 1; i < sources.length; i++) {
    col = col.merge(ee.ImageCollection(sources[i]));
  }
  col = col
    .filterDate(w.start, w.end)
    .filterBounds(area_mangais)
    .filter(ee.Filter.lte('CLOUD_COVER', CONFIG.cloudCoverMax));
  return {col: col, sensors: sensors, start: w.start, end: w.end};
}

// ============================ AMOSTRA ============================
// Pontos de treino/validação com coordenadas guardadas como propriedades.
var points = mangal.merge(agua).merge(floresta).merge(aberto).map(function(f) {
  var c = ee.List(f.geometry().coordinates());
  return f.set({lon: c.get(0), lat: c.get(1)});
});

// ============================ VALIDAÇÃO ============================
// Atribui cada ponto a um fold com base no bloco espacial onde cai,
// para que pontos vizinhos nunca fiquem em treino e teste ao mesmo tempo.
function addFold(fc, seed) {
  return fc.map(function(f) {
    var ix = ee.Number(f.get('lon')).divide(CONFIG.blockDeg).floor().add(10000);
    var iy = ee.Number(f.get('lat')).divide(CONFIG.blockDeg).floor().add(10000);
    var h = ix.multiply(73856093).add(iy.multiply(19349663))
      .add(ee.Number(seed).multiply(83492791));
    return f.set('fold', h.mod(CONFIG.kFolds));
  });
}

// Uma repetição de validação cruzada por blocos; devolve a matriz de confusão
// agregada sobre todos os folds.
function crossValidate(sample, seed) {
  var s = addFold(sample, seed);
  var folds = ee.List.sequence(0, CONFIG.kFolds - 1).map(function(k) {
    var test = s.filter(ee.Filter.eq('fold', k));
    var train = s.filter(ee.Filter.neq('fold', k));
    var clf = ee.Classifier.smileRandomForest({
      numberOfTrees: CONFIG.cvTrees,
      minLeafPopulation: 2,
      bagFraction: 0.5,
      seed: seed
    }).train(train, 'landcover', PRED_BANDS);
    return test.classify(clf);
  });
  var preds = ee.FeatureCollection(folds).flatten();
  return preds.errorMatrix('landcover', 'classification', CONFIG.classes);
}

// Uma linha de métricas por repetição.
function repeatFeature(sample, seed) {
  var cm = crossValidate(sample, seed);
  var pa = cm.producersAccuracy().project([0]);   // recall, por classe
  var ua = cm.consumersAccuracy().project([1]);   // precisão, por classe
  var f1 = pa.multiply(ua).multiply(2).divide(pa.add(ua));
  var props = ee.Dictionary({repeat: seed, oa: cm.accuracy(), kappa: cm.kappa()})
    .combine(ee.Dictionary.fromLists(PA_KEYS, pa.toList()))
    .combine(ee.Dictionary.fromLists(UA_KEYS, ua.toList()))
    .combine(ee.Dictionary.fromLists(F1_KEYS, f1.toList()))
    .combine(ee.Dictionary.fromLists(CELL_KEYS, cm.array().toList().flatten()));
  return ee.Feature(null, props);
}

// ============================ ÁREAS ============================
// Área (km2) por classe dentro de cada feature de fc.
function areaTable(img, fc, nameProp, level, year) {
  return fc.map(function(f) {
    var d = ee.Image.pixelArea().divide(1e6).addBands(img.rename('class'))
      .reduceRegion({
        reducer: ee.Reducer.sum().group({groupField: 1, groupName: 'class'}),
        geometry: f.geometry(),
        crs: 'EPSG:32628',
        scale: 30,
        maxPixels: 1e13,
        tileScale: 8
      });
    var groups = ee.List(ee.Algorithms.If(d.contains('groups'), d.get('groups'), []));
    var zeros = ee.Dictionary.fromLists(KM_KEYS, ee.List.repeat(0, CONFIG.classes.length));
    var res = ee.Dictionary(groups.iterate(function(g, acc) {
      g = ee.Dictionary(g);
      var key = ee.String('km2_').cat(ee.Number(g.get('class')).toInt().format('%d'));
      return ee.Dictionary(acc).set(key, g.get('sum'));
    }, zeros));
    return ee.Feature(null, res.set('year', year).set('level', level)
      .set('name', f.get(nameProp)));
  });
}

// ============================ PIPELINE POR ANO ============================
function runYear(year) {
  var lc = landsatCollection(year);
  var prepared = lc.col.map(prepSrL8).map(addIndices);
  var image = prepared.median().clip(area_mangais).select(PRED_BANDS);
  var nObs = prepared.select('SR_B4').count().clip(area_mangais).rename('n_obs');
  var nImages = lc.col.size();

  // Amostra: valores dos preditores nos pontos (pontos em pixéis mascarados são descartados)
  var sample = image.sampleRegions({
    collection: points,
    properties: ['landcover', 'lon', 'lat'],
    scale: 30,
    tileScale: 4
  });

  // Validação cruzada por blocos, repetida
  var seeds = ee.List.sequence(1, CONFIG.cvRepeats);
  var reps = ee.FeatureCollection(seeds.map(function(s) {
    return repeatFeature(sample, s);
  }));

  var summary = {};
  ['oa', 'kappa'].concat(PA_KEYS, UA_KEYS, F1_KEYS).forEach(function(k) {
    summary['cv_' + k + '_mean'] = reps.aggregate_mean(k);
    summary['cv_' + k + '_sd'] = reps.aggregate_sample_sd(k);
  });
  CELL_KEYS.forEach(function(k) {
    summary['cv_' + k + '_mean'] = reps.aggregate_mean(k);
  });

  // Modelo final (todos os pontos)
  var classifier = ee.Classifier.smileRandomForest({
    numberOfTrees: CONFIG.trees,
    minLeafPopulation: 2,
    bagFraction: 0.5,
    seed: CONFIG.seed
  }).train(sample, 'landcover', PRED_BANDS);

  var explain = ee.Dictionary(classifier.explain());
  var importance = ee.Dictionary(explain.get('importance'));
  var impSum = ee.Number(importance.values().reduce(ee.Reducer.sum()));
  var relImportance = importance.map(function(key, val) {
    return ee.Number(val).multiply(100).divide(impSum);
  });

  var info = {
    year: year,
    window_start: lc.start,
    window_end: lc.end,
    sensors: lc.sensors,
    n_images: nImages,
    cloud_cover_max: CONFIG.cloudCoverMax,
    predictors: PRED_BANDS.join(','),
    trees_final: CONFIG.trees,
    trees_cv: CONFIG.cvTrees,
    cv_folds: CONFIG.kFolds,
    cv_repeats: CONFIG.cvRepeats,
    block_deg: CONFIG.blockDeg,
    oob_error: explain.get('outOfBagErrorEstimate'),
    n_points_total: points.size(),
    n_points_valid: sample.size()
  };
  CONFIG.classes.forEach(function(c) {
    info['n_points_' + c] = points.filter(ee.Filter.eq('landcover', c)).size();
    info['n_valid_' + c] = sample.filter(ee.Filter.eq('landcover', c)).size();
  });

  var metrics = ee.FeatureCollection([
    ee.Feature(null, ee.Dictionary(info).combine(summary))
  ]);
  var importanceFc = ee.FeatureCollection([
    ee.Feature(null, relImportance.set('year', year))
  ]);

  // Mapa
  var classified = image.classify(classifier);
  var filtered = classified.focalMode({
    radius: 2, kernelType: 'circle', units: 'pixels'});   // metricas de validação são sobre o mapa sem filtro

  // Áreas (km2) do mapa filtrado: total, por região administrativa e por AP
  var totalFc = ee.FeatureCollection([
    ee.Feature(area_mangais.geometry(), {name: 'total'})]);
  var areas = areaTable(filtered, totalFc, 'name', 'total', year)
    .merge(areaTable(filtered, admin1, 'name_1', 'admin1', year))
    .merge(areaTable(filtered, AP, 'NAME', 'protected_area', year));

  // Consola
  print('== ' + year + ' == metricas', metrics);
  print('== ' + year + ' == validacao por repeticao', reps);
  print('== ' + year + ' == importancia relativa (%)', relImportance);
  if (CONFIG.printAreas) {
    print('== ' + year + ' == areas (km2)', areas);
  }

  // Mapa
  Map.addLayer(nObs, {min: 1, max: 10, palette: ['red', 'yellow', 'green']},
    'N observacoes ' + year, 0);
  Map.addLayer(classified, {min: 1, max: 4, palette: PALETTE},
    'Coberto do Solo ' + year, 0);
  Map.addLayer(filtered, {min: 1, max: 4, palette: PALETTE},
    'Coberto do Solo Filtro ' + year, 1);

  // Exportações
  if (CONFIG.exportImages) {
    Export.image.toDrive({
      image: filtered.toByte(),
      description: 'mangal_gnb_' + year,
      folder: CONFIG.folder,
      scale: 30,
      crs: 'EPSG:32628',
      region: area_mangais.geometry(),
      fileFormat: 'GeoTIFF'
    });
  }
  if (CONFIG.exportTables) {
    var tables = {
      metrics: metrics, cv_repeats: reps,
      importance: importanceFc, areas: areas
    };
    Object.keys(tables).forEach(function(name) {
      Export.table.toDrive({
        collection: tables[name],
        description: 'mangal_gnb_' + name + '_' + year,
        folder: CONFIG.folder,
        fileFormat: 'CSV'
      });
    });
  }
}

// ============================ EXECUÇÃO ============================
Map.setOptions('SATELLITE');
Map.centerObject(area_mangais, 9);
Map.addLayer(admin1, {}, 'Regioes Admin', 0);
Map.addLayer(area_mangais, {}, 'Area_cartografia', 0);
Map.addLayer(AP, {}, 'Areas Protegidas', 0);

CONFIG.years.forEach(runYear);
