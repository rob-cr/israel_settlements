// load and print SRTM data
var srtm = ee.Image("USGS/SRTMGL1_003");
print("SRTM", srtm);

// create and print study area polygon
var geom = ee.Geometry.Polygon(
[35.59635221732189,34.820895340177216,
33.75064909232189,31.67943515668432,
34.80533659232189,28.98686869477083,
36.45328581107189,31.71682506920078,
35.81607877982189,32.479982594525815,
36.40934049857189,33.14478445319269,
37.24430143607189,34.459341479933066,
36.38736784232189,34.820895340177216,
35.59635221732189,34.820895340177216]);

print("Study Area", geom);

// add both to map
Map.addLayer(srtm, {min: -500, max: 1000});
Map.addLayer(geom);


// export image to Google drive
Export.image.toDrive({
  image:srtm,
  region:geom,
  scale: 30,
  description: "SRTM-Il-Pal",
  maxPixels: 1e+9
});
