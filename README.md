# Global Partnerships for Coastal Multi-Hazard Risks

![R](https://img.shields.io/badge/R-%3E%3D4.0.0-blue)
![License](https://img.shields.io/badge/License-MIT-green)

This repository contains the R code implementation for the paper **"Understanding international cooperation potential in coastal multi-hazard governance"**. The code framework consists of three main components:

1. Multi-hazard Risk Assessment: High-resolution, comprehensive risk modeling for global coastal regions, incorporating earthquakes, landslides, flooding, and cyclones/typhoons
2. Risk Pattern Identification: Detection of continuous risk communities across 126 coastal countries and characterization of general risk patterns
3. Collaboration Network Analysis: Evaluation of international partnerships from an integrated perspective of risk similarity, geopolitical stance, and academic collaboration metrics

**_For the global risk community map and the full lists of country partnerships, please visit our <span style="color: #4A90E2">[interactive web](https://joyingl.github.io/cmhr/)</span>_**

## Data Availability

All analyses are based on publicly available datasets as detailed in the Supplementary Information of our paper, including natural hazard data, socioeconomic indicators, and international collaboration metrics. Readers can access these datasets through their respective official channels as referenced in our paper.

## Usage

### System Requirements

We recommend that the user's device meets the following system requirements.
- Operating system: Windows 10/11, macOS, or Linux
- R version: >= 4.0.0
- RStudio (recommended)
- Memory: >= 8GB RAM recommended

### Repository Structure

This repository is organized into two main components:

#### Data

- `01_study-area-global-coastal-regions/`: Coastal boundary definitions
- `02_hazard-modelling-susceptibility-assessment/`: Hazard modeling results
- `03_risk-powerlaw-statistical-analysis/`: Statistical analysis outputs
- `04_risk-communities-pattern-detection/`: Community detection results
- `05_risk-similarity-distance-matrices/`: Risk similarity measurements
- `06_bibliometrics-academic-collaboration-index/`: Academic collaboration data
- `07_voting-political-viewpoint-proximity/`: Political stance analysis
- `08_collaboration-partnership-synthesis/`: Final partnership results

#### Scripts

- `1_hazard_modeling/`: LightGBM models for multiple hazards
- `2_risk_assessment/`: Integration of hazard, vulnerability, and exposure
- `3_risk_coupling/`: Risk coupling analysis and power-law statistics
- `4_community_detection/`: Regional community detection algorithms
- `5_pattern_analysis/`: Risk pattern characterization
- `6_similarity_measurement/`: Risk similarity calculations
- `7_collaboration_network/`: Risk-based collaboration partnerships analysis
- `setup.R`: Package dependencies and environment setup

Each script is numbered according to the analysis workflow. Users should execute scripts sequentially starting from `setup.R`.

## Note

- This source code is provided for transparency and reproducibility of our research methods. 
- Raw data sources are detailed in our paper's Supplementary Information.
- Due to size limitations, processed data is not included in this repository. For access to intermediate data products, please contact the authors.
- For detailed methodology and results interpretation, please refer to our paper.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Citation

If you use this code in your research, please cite our paper:

## Contact

For questions about the code or paper, please:
- Open an issue on GitHub
- Contact: [Joying Lee](mailto:joyinghua@gmail.com)
