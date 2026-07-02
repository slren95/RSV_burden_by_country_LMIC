"""将 Table S10/S11 原始 CSV 整理为每地区一行，仅保留点估计。"""

import csv
import re
from pathlib import Path

import pycountry

RAW_DIR = Path(__file__).parent / "extracted"
OUT_DIR = RAW_DIR

# GBD 超区域 / 区域（非国家，ISOCountry 留空）
NON_COUNTRIES = {
    "Global",
    "Central Europe, Eastern Europe, and Central Asia",
    "High-income",
    "Latin America and Caribbean",
    "North Africa and Middle East",
    "Southeast Asia, East Asia, and Oceania",
    "Sub-Saharan Africa",
    "Central Asia",
    "Central Europe",
    "Eastern Europe",
    "Australasia",
    "Caribbean",
    "East Asia",
    "High-income Asia-Pacific",
    "High-income Asia- Pacific",
    "High-income North America",
    "Oceania",
    "South Asia",
    "Southeast Asia",
    "Western Europe",
    "Andean Latin America",
    "Central Latin America",
    "Central sub- Saharan Africa",
    "Eastern sub- Saharan Africa",
    "Southern Latin America",
    "Southern sub- Saharan Africa",
    "Tropical Latin America",
    "Western sub- Saharan Africa",
}

# GBD 国家名称与 ISO 3166-1 alpha-3 不一致时的手动映射
GBD_ISO_ALIASES = {
    "Macedonia": "MKD",
    "South Korea": "KOR",
    "North Korea": "PRK",
    "The Bahamas": "BHS",
    "The Gambia": "GMB",
    "Cote d'Ivoire": "CIV",
    "Congo (Brazzaville)": "COG",
    "Democratic Republic of the Congo": "COD",
    "Czech Republic": "CZE",
    "Bolivia": "BOL",
    "Iran": "IRN",
    "Laos": "LAO",
    "Moldova": "MDA",
    "Russia": "RUS",
    "Syria": "SYR",
    "Taiwan": "TWN",
    "Tanzania": "TZA",
    "Turkey": "TUR",
    "United States": "USA",
    "United Kingdom": "GBR",
    "Venezuela": "VEN",
    "Vietnam": "VNM",
    "Swaziland (eSwatini)": "SWZ",
    "Cape Verde": "CPV",
    "Federated States of Micronesia": "FSM",
    "Guinea- Bissau": "GNB",
    "Virgin Islands, U.S.": "VIR",
    "Palestine": "PSE",
    "Timor-Leste": "TLS",
    "Sao Tome and Principe": "STP",
}

COL_NAMES = [
    "rate_1990",
    "rate_1995",
    "rate_2000",
    "rate_2005",
    "rate_2010",
    "rate_2016",
    "annual_change_pct",
    "volume_2016",
    "pct_change_volume",
]

HEADER_MARKERS = (
    "Table S",
    "Age standardised",
    "Estimates based",
    "Annual rate",
    "Volume of",
    "Total percent",
    "structure",
    "per capita",
    "percentage",
    "in thousands",
    "Figure",
    "of change in",
    "standardised",
)


def normalize_number(text: str) -> str:
    text = text.strip()
    if not text:
        return ""

    if "(" in text:
        text = text.split("(", 1)[0].strip()

    text = text.replace("·", ".").replace("−", "-").replace("–", "-")
    text = re.sub(r"\s+", "", text)
    return text


def is_numeric_point(value: str) -> bool:
    if not value or value.strip().startswith("("):
        return False

    point = normalize_number(value)
    if not point or not re.search(r"\d", point):
        return False

    cleaned = point.replace(" ", "")
    return bool(re.match(r"^-?\d+\.?\d*$", cleaned))


def cell_text(row, idx: int) -> str:
    if idx >= len(row):
        return ""
    return row[idx].strip()


def location_text(row) -> str:
    parts = [cell_text(row, 0), cell_text(row, 1)]
    return " ".join(p for p in parts if p).strip()


def row_text(row) -> str:
    return " ".join(cell_text(row, i) for i in range(len(row)))


def is_header_row(row) -> bool:
    text = row_text(row)
    if not text:
        return True
    if "1990" in text and "2016" in text and not location_text(row):
        return True
    return any(m in text for m in HEADER_MARKERS)


def extract_point_values(row) -> list[str]:
    values = []
    for i in range(2, len(row)):
        val = cell_text(row, i)
        if is_numeric_point(val):
            values.append(normalize_number(val))
    return values


def is_ci_row(row) -> bool:
    loc = location_text(row)
    values = extract_point_values(row)
    ci_cells = [
        cell_text(row, i)
        for i in range(2, len(row))
        if cell_text(row, i).startswith("(")
    ]
    return not loc and not values and bool(ci_cells)


def has_point_data(row) -> bool:
    values = extract_point_values(row)
    loc = location_text(row)
    if loc:
        return len(values) >= 1
    return len(values) >= 3


def normalize_location_name(name: str) -> str:
    """修正 PDF 提取造成的地区名乱码。"""
    if name.startswith("Virgin Islands"):
        return "Virgin Islands, U.S."
    return re.sub(r"\s+", " ", name).strip()


def lookup_iso_country(location: str) -> str:
    """国家返回 ISO 3166-1 alpha-3，非国家返回空字符串。"""
    name = normalize_location_name(location)
    if name in NON_COUNTRIES:
        return ""
    if name in GBD_ISO_ALIASES:
        return GBD_ISO_ALIASES[name]
    try:
        return pycountry.countries.search_fuzzy(name)[0].alpha_3
    except LookupError:
        return ""


def build_location(rows, idx: int) -> str:
    loc = location_text(rows[idx])
    j = idx + 1

    while j < len(rows):
        nxt = rows[j]
        if is_header_row(nxt) or has_point_data(nxt):
            break
        if is_ci_row(nxt):
            frag = location_text(nxt)
            if frag:
                loc = f"{loc} {frag}".strip()
            break
        frag = location_text(nxt)
        if frag:
            loc = f"{loc} {frag}".strip()
            j += 1
            continue
        break

    return re.sub(r"\s+", " ", loc).strip(" ,")


def parse_table(raw_path: Path) -> list[dict]:
    with raw_path.open(encoding="utf-8-sig") as f:
        rows = list(csv.reader(f))

    records = []
    i = 0
    while i < len(rows):
        row = rows[i]
        if is_header_row(row) or is_ci_row(row):
            i += 1
            continue

        if not has_point_data(row):
            i += 1
            continue

        loc = build_location(rows, i)
        if not loc:
            i += 1
            continue

        values = extract_point_values(row)
        record = {
            "location": loc,
            "ISOCountry": lookup_iso_country(loc),
        }
        for name, val in zip(COL_NAMES, values):
            record[name] = val
        for name in COL_NAMES[len(values) :]:
            record[name] = ""

        records.append(record)
        i += 1

    return records


def save_records(records: list[dict], out_path: Path) -> None:
    fieldnames = ["location", "ISOCountry", *COL_NAMES]
    with out_path.open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(records)


def main():
    tasks = [
        (RAW_DIR / "Table_S10_outpatient.csv", OUT_DIR / "Table_S10_outpatient_flat.csv"),
        (RAW_DIR / "Table_S11_inpatient.csv", OUT_DIR / "Table_S11_inpatient_flat.csv"),
    ]

    for raw_path, out_path in tasks:
        if not raw_path.exists():
            print(f"跳过（文件不存在）: {raw_path}")
            continue

        records = parse_table(raw_path)
        save_records(records, out_path)
        print(f"{out_path.name}: {len(records)} 个地区")

        countries = sum(1 for r in records if r["ISOCountry"])
        print(f"  其中国家 {countries} 个，非国家 {len(records) - countries} 个")

        print("  预览:")
        for rec in records[:5]:
            iso = rec["ISOCountry"] or "   "
            print(
                f"    {iso} | {rec['location'][:40]:40s} | "
                f"2016={rec['rate_2016']:>8s}"
            )


if __name__ == "__main__":
    main()
