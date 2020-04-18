"addr:postcode" from OSM-Data; [out:json][timeout:25];
// gather results
(
  // query part for: “"addr:postcode"=*”
  node["addr:postcode"]({{bbox}});
  way["addr:postcode"]({{bbox}});
  relation["addr:postcode"]({{bbox}});
);
// print results
out body;
>;
out skel qt;
