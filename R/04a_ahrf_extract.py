#!/usr/bin/env python3
"""
04a_ahrf_extract.py ------------------------------------------------------

One-time conversion of the Area Health Resources File (AHRF) county SAS
dataset into a slim CSV of the columns this study uses.

Why a Python step in an otherwise-R pipeline: the AHRF ships as a 107 MB
sas7bdat with 4,306 columns. Reading it needs `haven` in R or `pyreadstat` in
Python. Converting once to a ~200 KB CSV keeps the large binary out of version
control and makes every downstream R script fast and dependency-light.

Source:  https://data.hrsa.gov/DataDownload/AHRF/AHRF_SAS_2022-2023.zip
Vintage: AHRF 2022-2023, which carries 2021-stamped provider counts. This is
         the release that matches our 2021 analytic year; the newer 2024-2025
         file only goes back to 2022 and would NOT align.
Note:    the zip includes a click-through data use acknowledgment
         (2022-2023_AHRFDUA.doc). Read it before redistributing the raw file.

Usage:   python R/04a_ahrf_extract.py
"""

import pyreadstat
import pandas as pd

SRC = "data/ahrf/AHRF_SAS_2022-2023/ahrf2023.sas7bdat"
OUT = "data_derived/ahrf_2021_county.csv"

COLS = {
    "fips_st_cnty":            "fips",
    "cnty_name":               "ahrf_county_name",
    # --- eye care workforce, 2021 vintage ---
    "md_nf_ophth_21":          "ophth_total",        # all non-federal ophthalmologists
    "md_nf_ophth_all_pc_21":   "ophth_patient_care", # patient-care ophthalmologists
    "md_nf_ophth_pc_rsdnt_21": "ophth_residents",    # hospital residents (program capacity)
    "md_nf_ophth_teach_21":    "ophth_teaching",     # primary activity: teaching
    "opto_npi_21":             "optom_npi",          # optometrists with an NPI
    # --- rurality / geography controls ---
    "rural_urban_contnm_13":   "rucc_2013",
    "cbsa_ind_20":             "cbsa_ind",           # 0 not, 1 metro, 2 micro
    "cens_rural_popn_20":      "rural_pop_2020",
}

_, meta = pyreadstat.read_sas7bdat(SRC, metadataonly=True)
available = [c for c in COLS if c in meta.column_names]
missing = [c for c in COLS if c not in meta.column_names]
if missing:
    print("WARNING: columns absent from this AHRF vintage:", missing)
    # county-name column varies by release; find it rather than guessing
    if "cnty_name" in missing:
        cands = [c for c in meta.column_names if "cnty_name" in c.lower()]
        print("  county-name candidates:", cands[:5])

df, _ = pyreadstat.read_sas7bdat(SRC, usecols=available)
df = df.rename(columns={k: v for k, v in COLS.items() if k in available})

df["fips"] = df["fips"].astype(str).str.zfill(5)

print(f"rows: {len(df)}")
for c in ["ophth_total", "ophth_patient_care", "ophth_residents",
          "ophth_teaching", "optom_npi"]:
    if c in df:
        print(f"  {c:20s} nonmissing={df[c].notna().sum():5d}  "
              f"sum={df[c].sum():10.0f}  n>0={int((df[c] > 0).sum()):5d}")

df.to_csv(OUT, index=False)
print("wrote", OUT)
