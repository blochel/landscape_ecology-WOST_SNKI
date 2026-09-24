# Everglades Wetland Bird Nesting Study
### Florida Snail Kite (*Rostrhamus sociabilis plumbeus*) & Wood Stork (*Mycteria americana*)

---

## 1. Study Species & System

**System:** The Freshwater Greater Everglades

**Species:**
- Florida Snail Kite (*Rostrhamus sociabilis plumbeus*)
- Wood Stork (*Mycteria americana*)

The history of Everglades hydrology is characterized by a transition from a vast, interconnected watershed to a highly compartmentalized and human-managed network of wetland units, altering the timing, depth, and connectivity of water across the landscape and disrupting processes that maintain wetland function (Harvey et al. 2019). Across this approximately 1.0-million-ha, low-gradient landscape, small differences in elevation produce substantial variation in water depth and hydroperiod, while widespread wet-season inundation contracts into increasingly shallow and isolated water bodies during the dry season.

The endemic **Florida Snail Kite** is considered endangered because altered hydrology negatively influenced the distribution of apple snails (*Pomacea* sp.), its specialized prey. The **Wood Stork** was considered endangered in the state of Florida until 2026, when its status was shifted to threatened.

Both species are considered **indicator species** in the system, but for different reasons:
- Snail Kites reflect freshwater conditions that sustain accessible apple snails.
- Wood Storks reflect seasonal recession across a heterogeneous landscape which concentrates fish in shallow ponds (Benscoter et al. 2023; Frederick et al. 2009).

---

## 2. Problem Statement

Hydrologic conditions are critically important in predicting where highly mobile wetland birds initiate nesting and whether they successfully reproduce. Seasonal climate variation and human-managed flood control can produce water levels that are too high or too low for Snail Kites and Wood Storks, reducing nesting activity and reproductive success.

**Research Questions:**
1. How do climate, land cover, and hydrology predict the **number of nests** found per species within a given season?
2. How do these variables predict whether an identified nest **successfully fledges young**?

A better understanding of how these wetland birds respond to hydrologic and landscape conditions may help managers evaluate when and where conditions are favorable for nesting, and inform future management and restoration assessments.

---

## 3. Data Description

### Nest Data
- **Wood Stork:** Long-term Everglades Wading Bird Project data obtained via the R package `wader`, including nest locations, monitoring histories, reproductive outcomes, and field observations.
- **Snail Kite:** A 30-year UF Snail Kite database (not publicly available), providing nest locations, monitoring histories, reproductive outcomes, suspected parentage, and other field-collected information.

### Hydrologic Data
- Daily hydrologic data from the **Everglades Depth Estimation Network (EDEN)** via the `edenR` package (Haider et al. 2020; Yenni et al. 2026).
- EDEN integrates water-level-gage observations, hydrologic models, spatial interpolation, and elevation data to estimate daily water depth at **400 × 400 m resolution** (with a 50 × 50 m option evaluated where practical).

### Land Cover
- FDEP/SFWMD polygons reclassified into ecologically interpretable categories: open water, freshwater marsh, wet prairie, shrub wetland, forested wetland, mangrove, and developed land.
- The current statewide compilation represents approximately 2020–2023 and will be treated as static or replaced with temporally matched SFWMD epochs.

### Additional Covariates
- **Elevation:** EDEN-compatible digital elevation model (static terrain covariate).
- **Vegetation structure:** LANDFIRE canopy cover and vegetation density.
- **Climate:** Temperature and precipitation from ~4 km gridMET data.

---

## 4. Expected Deliverables

- Answers to the research questions above, providing management-relevant inference on environmental conditions associated with nesting effort and reproductive success.
- **Habitat suitability maps** for both species.
- **Classified rasters** of landscape metrics of interest.
- **Spatial models** of nesting effort and success.
- The first comparative habitat suitability analysis of Snail Kites and a wading bird since Curnutt (2000).

---

## 5. Collaboration Plan

Alex and Brooke will meet for **30–60 minutes after class each Monday** to evaluate progress, discuss deadlines, and identify roadblocks, with longer Thursday meetings held as needed.

| Team Member | Species Expertise | Primary Responsibilities |
|---|---|---|
| Brooke | Florida Snail Kite | Raster acquisition & classification |
| Alex | Wood Stork | Environmental data extraction & final spatial modeling |

Both members will collaborate on the final report and presentation, dividing species-specific sections while jointly writing comparative sections.