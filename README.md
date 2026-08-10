# Focus Group Digital Twins Simulation Reproduction Repository

This repository provides the data and code to reproduce the results in the paper, Esterling, Kevin M., Ben Treves, Kelton Adey and Euchan Jang. (2026) "Using LLM Digital Twin Simulation for Synchronous Research: With an Application to the Structured Group Design," paper presented at the Annual Meeting of the Western Political Science Association, April 2, 2026. The paper is [included in this repository](https://github.com/kesterling/FGDT-results/blob/main/Using%20LLM%20Digital%20Twin%20Simulation%20to%20Evaluate%20the%20Emergent%20Properties%20of%20Human%20Group%20Interaction.pdf).

If you have any questions about running this code, please email Kevin Esterling <kevin.esterling@ucr.edu>. 

## Instructions

To reproduce the figures in the paper, follow these simple steps:

1. Download this repository as a zip file and unpack.
2. Open the file FGDT_figure_generator_original.R in an IDE such as RStudio and run as a source. This creates the figures in the paper and saves a file results_original.txt which has the results needed to reproduce Table A1.
3. Optionally, you can run the file FGDT_figure_generator_original.R which provides the replication figures and results_replication.txt file that is also needed for Table A1.
4. IMPORTANT: Use your GUI to set the IDE working directory to the "Results" folder
5. Run the R file. You might need to install the required packages

Note: if you do not set your IDE working directory to the Results folder, you will receive an error. I also notice that when running the file as a source in RStudio, it seems that RStudio skips over some commands for some reason, so in RStudio it works better to highlight the full script and then press "Run."

## Listing of files and folders

- The replication files are in the "Results" folder
- "FGDT_figure_generator_original.R" is the code that generates the figures for the original experiments
- "FGDT_figure_generator_replication.R" is the code that generates the figures for the replication experiments
- Each of these figure generators contains a list of helper scripts that generate the figures themselves and references the data by subdirectory name
- "figures" is the author generated figures and results files that appear in the paper
