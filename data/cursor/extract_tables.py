"""使用 pdfplumber 从 GBD 2016 补充材料 PDF 中提取 Table S10 和 Table S11。"""

import csv
from pathlib import Path

import pdfplumber

PDF_PATH = Path(__file__).parent / "mmc1_GBD_2016_TableS10_S11.pdf"
OUT_DIR = Path(__file__).parent / "extracted"


def clean_cell(cell):
    if cell is None:
        return ""
    return str(cell).replace("\n", " ").strip()


def is_header_row(row):
    text = " ".join(clean_cell(c) for c in row if c)
    markers = (
        "Table S",
        "Age standardised for international",
        "Estimates based on national age",
        "Age standardised outpatient",
        "Age standardised inpatient",
        "Annual rate of change",
        "Volume of outpatient",
        "Volume of inpatient",
        "structure",
        "per capita",
        "percentage",
        "in thousands",
    )
    return any(m in text for m in markers) or text == ""


def is_column_header_row(row):
    text = " ".join(clean_cell(c) for c in row if c)
    return "1990" in text and "2016" in text


def merge_tables_from_pages(pages):
    all_rows = []
    first_page = True

    for page in pages:
        tables = page.extract_tables()
        if not tables:
            continue

        main_table = max(tables, key=len)
        skip = 0

        if first_page:
            first_page = False
        else:
            for i, row in enumerate(main_table[:8]):
                if is_header_row(row) or is_column_header_row(row):
                    skip = i + 1
                else:
                    break

        all_rows.extend(main_table[skip:])

    return all_rows


def save_table(rows, filepath):
    filepath.parent.mkdir(parents=True, exist_ok=True)
    with filepath.open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.writer(f)
        for row in rows:
            writer.writerow([clean_cell(c) for c in row])


def extract_table_s10(pdf):
    # Table S10 分布在第 1–6 页（索引 0–5）
    return merge_tables_from_pages(pdf.pages[0:6])


def extract_table_s11(pdf):
    # Table S11 从第 8 页开始（索引 7），第 7 页仅为标题
    return merge_tables_from_pages(pdf.pages[7:13])


def main():
    if not PDF_PATH.exists():
        raise FileNotFoundError(f"找不到 PDF 文件: {PDF_PATH}")

    with pdfplumber.open(PDF_PATH) as pdf:
        print(f"PDF 共 {len(pdf.pages)} 页")

        table_s10 = extract_table_s10(pdf)
        table_s11 = extract_table_s11(pdf)

        s10_path = OUT_DIR / "Table_S10_outpatient.csv"
        s11_path = OUT_DIR / "Table_S11_inpatient.csv"

        save_table(table_s10, s10_path)
        save_table(table_s11, s11_path)

        print(f"Table S10: {len(table_s10)} 行 -> {s10_path}")
        print(f"Table S11: {len(table_s11)} 行 -> {s11_path}")

        print("\n--- Table S10 前 5 行预览 ---")
        for row in table_s10[:5]:
            print([clean_cell(c)[:50] for c in row[:5]])

        print("\n--- Table S11 前 5 行预览 ---")
        for row in table_s11[:5]:
            print([clean_cell(c)[:50] for c in row[:5]])


if __name__ == "__main__":
    main()
