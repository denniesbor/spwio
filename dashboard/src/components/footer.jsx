import React from "react";
import { useAppContext } from "../AppContext";

function Footer() {
  const { regionNames, scenarioRegions, shocksNAICS, sidebarVal } = useAppContext();
  const shouldFilterRegions = scenarioRegions && scenarioRegions.length > 0;

  return (
    <footer>
      <div className="footer-left">
        <h3>Glossary</h3>
        <div className="glossary-terms">
          {Object.entries(regionNames)
            .filter(
              ([key]) => !shouldFilterRegions || scenarioRegions.includes(key)
            )
            .map(([key, value]) => (
              <div key={key}>{`${key}: ${value}`}</div>
            ))}
        </div>
      </div>
      <div className="footer-right">
        {sidebarVal !== "nerc" ? (
          <>
            <h3>NAICS Code</h3>
            <div className="glossary-terms">
              {Object.entries(shocksNAICS["industry"]).map(([key, value]) => (
                <div key={key}>{`${key}: ${value}`}</div>
              ))}
            </div>
          </>
        ) : (
          <p>
            This material is based upon work supported by the NSF National
            Center for Atmospheric Research, which is a major facility sponsored
            by the U.S. National Science Foundation under Cooperative Agreement
            No. 1852977. The project upon which this website is based was funded
            through the NSF NCAR Early-Career Faculty Innovator Program under
            the same Cooperative Agreement.
          </p>
        )}
      </div>
    </footer>
  );
}

export default Footer;
