## 🙏 Acknowledgements
An initial version of the python code was kindly provided by my thesis supervisor, **Prof. David Jiménez Gómez (PhD, MIT)**. Building on that foundation, I developed, expanded, and adapted most of the code presented here as part of my Bachelor's thesis.

# Thesis Experiment Code

This repository contains the code used in my Bachelor's thesis experiment on the effect of **default options** and **behavioral prompts** on personal finance decisions simulated through ChatGPT.

## 📂 Files
- `experiment_code.py` — main Python script that runs the experiment, interacts with the OpenAI API, processes responses, and saves data outputs.
- `prompt_tfg.json` — JSON file with the text prompts used in the experiment. It defines different system roles (baseline ChatGPT, human, human with procrastination tendency) and user conditions (with or without automatic enrollment).
- `Data_analysis_code.R` — R script used for cleaning, aggregating, and analyzing the experimental results (not detailed here but included in the repo).
- Outputs — the script automatically generates a `/data` folder with CSV and Excel files containing the cleaned results.

## ⚙️ What the Python code does
The Python script:
1. Loads the system and user prompts from `prompt_tfg.json`.
2. Runs interactions with the OpenAI API under six treatment combinations (3 system personalities × 2 user conditions).
3. Extracts numeric answers from ChatGPT’s responses, checks their validity, and classifies them (participation, default choice, contribution rate, investment type).
4. Appends the results into CSV/Excel files for later use in econometric models.
5. Automates the process across multiple batches to build a large dataset.

## 🔑 Requirements
- Python 3.9+
- Libraries: `pandas`, `openai`, `json`, `requests`, `dotenv`, `xlsxwriter`
- An OpenAI API key (set as an environment variable, e.g., via `.env`)

## 🚀 How to run
1. Place `prompt_tfg.json` in the same folder as `experiment_code.py`.
2. Save your OpenAI API key in an environment variable or `.env` file.
3. Run the Python script:
   ```bash
   python experiment_code.py
