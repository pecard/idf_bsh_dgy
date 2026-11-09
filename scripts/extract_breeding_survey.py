"""Extrai as tabelas de resultados do relatorio de monitorizacao de nidificacao
(Breeding Raptor Nest Monitoring 2026, BSH + DGY) para um xlsx tidy em ingles,
lido depois por explore_bsh_dgy_comparison.R (seccao "Raptor Breeding Survey").

Uso: python3 scripts/extract_breeding_survey.py <relatorio.docx> [saida.xlsx]
Dependencias: python-docx, openpyxl. Os rotulos de tabela referem-se a
numeracao do relatorio original (Tabelas 8-16)."""
import re, sys
import docx
import openpyxl

SPECIES_FIX = {
    "Eagle Owl": "Eurasian Eagle-Owl", "Eurasian Eagle Owl": "Eurasian Eagle-Owl",
    "Vulture": "Egyptian Vulture",
}
def sp(x):
    x = x.strip()
    return SPECIES_FIX.get(x, x)

def rows(t):
    out = []
    for r in t.rows:
        seen, cells = [], []
        for c in r.cells:
            if c._tc in seen: continue
            seen.append(c._tc); cells.append(c.text.strip().replace("\n", " "))
        out.append(cells)
    return out

def num(x):
    m = re.search(r"-?\d+(?:\.\d+)?", x.replace(",", ""))
    return float(m.group()) if m else None

def find_table(d, header_start, caption_hint=None):
    for t in d.tables:
        r = rows(t)
        if r and r[0][:len(header_start)] == header_start:
            yield t

def main(src, dst):
    d = docx.Document(src)
    T = d.tables

    # Tabelas 8/9: inventario de ninhos (DGY = idx 9, BSH = idx 10)
    inv_hdr = ["Observation Point", "Nest No", "Active Nest", "Nest Easting", "Nest Northing", "Species", "Former Nest ID"]
    inv = []
    for farm, idx in (("DGY", 9), ("BSH", 10)):
        r = rows(T[idx]); assert r[0] == inv_hdr, r[0]
        for c in r[1:]:
            if c[0].upper() == "TOTAL": continue
            inv.append([farm, c[0], int(c[1]), "Yes" if c[2].lower() == "x" else "No",
                        num(c[3]), num(c[4]), sp(c[5]) or None, c[6] or None])

    # Tabelas 11/12: sucesso reprodutor de ninhos activos (DGY idx 12, BSH idx 13)
    succ = []
    for farm, idx in (("DGY", 12), ("BSH", 13)):
        r = rows(T[idx]); h = r[0]
        late_month = h[5]  # August (DGY) / September (BSH)
        for c in r[1:]:
            succ.append([farm, int(c[0]), sp(c[1]), c[2] or None, c[3] or None, c[4] or None,
                         c[5] or None, late_month, c[6]])

    # Tabelas 10/13: ninhos de peneireiro-comum em pilares da OHTL (BSH)
    pyl = {}
    for c in rows(T[11])[1:]:
        pyl[c[0]] = [c[0], num(c[1]), num(c[2])]
    pylons = []
    for c in rows(T[14])[1:]:
        base = pyl[c[0]]
        pylons.append(["BSH", c[0], "Common Kestrel", base[1], base[2], c[1] or None, c[2] or None, c[3] or None, c[4]])

    # Tabela 16: ninhos ocupados vs turbinas
    tur = []
    for c in rows(T[17])[1:]:
        buf = [x.strip() for x in c[5].split(",") if x.strip()]
        tur.append(["DGY" if c[0] == "DZH" else "BSH" if c[0] == "BAS" else c[0],
                    sp(c[1]), c[2], c[3], num(c[4]), len(buf), c[5] or None])

    # Tabela 15: comparacao com o ano anterior
    comp, farm = [], None
    for c in rows(T[16])[1:]:
        if c[1] == "" and c[2] == "":
            farm = {"Dzhankeldy": "DGY", "Bash": "BSH"}[c[0]]; continue
        comp.append([farm, sp(c[0]) if c[0] != "Unidentified/Empty" else "Unidentified/Empty", c[1], c[2]])

    wb = openpyxl.Workbook(); wb.remove(wb.active)
    def sheet(name, header, data):
        ws = wb.create_sheet(name); ws.append(header)
        for r in data: ws.append(r)
    sheet("Nest_inventory", ["farm", "observation_point", "nest_no", "active_nest", "easting", "northing", "species", "former_nest_id"], inv)
    sheet("Breeding_success", ["farm", "nest_no", "species", "april", "may", "june", "late_visit", "late_visit_month", "breeding_success"], succ)
    sheet("Pylon_kestrel_nests", ["farm", "nest_id", "species", "easting", "northing", "april", "may", "june", "breeding_success"], pylons)
    sheet("Nests_vs_turbines", ["farm", "species", "nest_id", "nearest_turbine", "distance_m", "n_turbines_2km", "turbines_2km_buffer"], tur)
    sheet("Previous_year_comparison", ["farm", "species", "previous_year", "current_year"], comp)
    wb.save(dst)
    print(f"{dst}: inventory {len(inv)}, success {len(succ)}, pylons {len(pylons)}, turbines {len(tur)}, comparison {len(comp)}")

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else "inputs/breeding_survey_2026.xlsx")
