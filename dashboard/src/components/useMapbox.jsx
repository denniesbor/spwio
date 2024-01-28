// useMapbox.js
import { useEffect } from "react";
import mapboxgl from "mapbox-gl";
import { useAppContext } from "../AppContext";

const useMapbox = (containerRef, options, sidebarVal) => {
  const {
    fillColorArray,
    geoData,
    setMouseLocation,
    setRegionName,
    setMouseExit,
    setMapLoaded,
    filteredGeoData,
  } = useAppContext();

  useEffect(() => {
    setMapLoaded(false);
    const map = new mapboxgl.Map({
      container: containerRef.current,
      ...options,
    });

    // make a pointer cursor
    map.getCanvas().style.cursor = "default";

    // set map bounds to the continental US
    map.fitBounds([
      [-133.2421875, 24.972741],
      [-47.63671875, 52.696361],
    ]);
    // console.log(geoData);
    map.on("load", () => {
      // NERC
      map.addSource("ferc_regions", {
        type: "geojson",
        data: sidebarVal === "nerc" ? geoData : filteredGeoData,
      });

      map.addLayer({
        id: "REGIONS",
        type: "fill",
        source: "ferc_regions",
        paint: {
          "fill-outline-color": "blue",
          "fill-opacity": 0,
        },
      });

      map.addLayer({
        id: "LINE",
        type: "line",
        source: "ferc_regions",
        layout: {},
        paint: {
          "line-color": "white",
          "line-width": 1,
        },
      });

      // Get the region name on Mouse Move
      map.on("mousemove", "REGIONS", (e) => {
        if (e.features.length > 0) {
          setRegionName(e.features[0].properties.REGIONS);
          setMouseLocation({ x: e.point.x, y: e.point.y });
          setMouseExit(false);
        }
      });

      map.on("mouseleave", "REGIONS", (e) => {
        setMouseExit(true);
      });

      //  Handle selected Scenarios
      if (map.getLayer("regions")) {
        map.removeLayer("regions");
        map.removeSource("regions");
      }

      map.addLayer({
        id: "regions",
        type: "fill",
        source: "ferc_regions",
        paint: {
          "fill-color": fillColorArray,
          "fill-opacity": 0.8,
        },
      });

      // window.addEventListener("resize", () => {
      //   map.resize();
      // });
    });

    return () => {
      // window.removeEventListener("resize", () => map.resize());
      map.remove();
      setTimeout(() => {
        setMapLoaded(true);
      }, 1000);
    };
  }, [sidebarVal, geoData, filteredGeoData]);
};

export default useMapbox;
