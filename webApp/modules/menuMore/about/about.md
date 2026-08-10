
### Version history

Version 0.9.10 (December 2020)
- First release

Version 0.9.11 (April 2021)
- *[General]* Developer documentation
- *[General]* Package manager "renv"
- *[General]* Handling categorical outputs (classification surrogate models)
- *[Uncertainty Quantification]* Adding distribution fitting for modeling uncertainty on inputs & dependence modeling via copulas
- *[Sensitivity Analysis]* Adding Shapley effects for test cases with dependent inputs

Version 0.10.0 (September 2022)
- *[General]* Calibration of numerical codes with experimental data
- *[General]* Composite outputs from analytical formulas
- *[Sequential Optimization]* Improve numerical stability and robustness
- *[General]* Increasing test coverage with tests for server functions
- *[Design of Experiments]* Improve speed and functionalities of DOE visualization
- *[Preliminary Exploration]* Add rule-based models (SIRUS)
- *[Optimization]* Plugin to use an external optimizer directly interacting with a numerical simulator

Version 1.0.0 (July 2023)
- *[General]* Save and load projects
- *[General]* Export surrogate models to Python
- *[General]* Plugin to perform external calibration of numerical simulators with experimental data

Version 1.0.1 (February 2025)
- *[General]* Docker files (for Lagun and simulations Launcher)
- *[General]* Save and load projects (extension to similations launcher + performance improvements)
- *[General]* Each parallel coordinate plot is accompanied by a scatter plot matrix, and they are synchronized with each other
- *[Direct optimization]* 'Use a previous optimization' improved (restore not only defintion, but also results)
- *[Simulations Launcher]* In simulator definition, new attributes 'Vector support' and 'Vector size'
- *[Simulations Launcher]* Ability to use the query string from the Lagun URL to specify how to connect the simulations launcher
- *[Surrogate model]* Add 'surrogate model pluggin system'
- *[Tests]* Upgraded from shinytest to shinytest2. Improved pipeline with speed up

Version 1.0.2 (March 2025)
- *[General]* Docker images for lagun and tests are directly built during ci and stored in gitlab container registries.

Version 1.0.3 (January 2026)
- *[General]* New menu 'Logs' to see LAGUN Logs (with daily rolling)
- *[General]* Save and load projects (new performance improvements)
- *[General]* 'Auto Saving' feature (after various actions, current study is automatically saved in a dedicated file)
- *[Direct optimization]* Add 'Feasible set learning problem' (inversion)
- *[Simulations Launcher]* Send simulator messages to a dedicated file 'SimulationsLauncher_out.txt'
- *[Surrogate export]* fix bug for negative output and status output. Add export of kriging with linear trend
- *[Direct optimization]*' Ability to define multiple initial values


Please contact **saf.lagun@safrangroup.com**, **ifpen.lagun@ifpen.fr**, or create a post at **https://ifpen-discourse.appcollaboratif.fr/c/lagun/** for suggestions, comments or bugs.
