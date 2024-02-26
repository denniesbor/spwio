# NAICS code and industry concordance
NAICSIndustries = {
    "11": "Agriculture, Forestry, Fishing and Hunting",
    "21": "Mining, Quarrying, and Oil and Gas Extraction",
    "22": "Utilities",
    "23": "Construction",
    "31": "Manufacturing",
    "42": "Wholesale Trade",
    "44": "Retail Trade",
    "48": "Transportation and Warehousing",
    "51": "Information",
    "FIRE": "Finance, insurance, real estate, rental, and leasing",
    "PROF": "Professional and business services",
    "6": "Educational services, health care, and social assistance",
    "7": "Arts, entertainment, recreation, accommodation, and food services",
    "81": "Other Services (except Public Administration)",
    "G": "Government",
}

# Create a sorting key based on NAICSIndustries keys
sort_key = {code: i for i, code in enumerate(NAICSIndustries.keys())}
