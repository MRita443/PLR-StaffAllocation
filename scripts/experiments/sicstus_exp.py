import subprocess
import csv
import re
import os
from itertools import product
import tempfile

# --- Configuration of labeling options ---
VAR_ORDERINGS = [
    "leftmost", "min", "max", "ff", "anti_first_fail", "occurrence",
    "ffc", "max_regret", "impact", "dom_w_deg"
]

VALUE_SELECTIONS = [
    "step", "enum", "bisect", "median", "middle", "value(selRandomValue)"
]

VALUE_ORDERINGS = ["up", "down"]  # Only applies to built-in value selection strategies

RESULTS_FILE = "results/results.csv"

def build_labeling(var, val, order=None):
    """Constructs the Prolog labeling list string including maximize(Utility)."""
    if val.startswith("value("):
        return f"[{var}, {val}]"
    else:
        return f"[{var}, {val}, {order}]"


def run_prolog(labeling_str):
    """Calls SICStus Prolog with a specific labeling strategy and returns its output."""
    goal = f"allocate_statistics({labeling_str}), halt."

    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as tmp_file:
        tmp_path = tmp_file.name

    try:
        with open(tmp_path, "w") as outfile:
            subprocess.run(
                ["sicstus", "-l", "./sicstus/main.pl", "--goal", goal],
                stdout=outfile,
                stderr=outfile,
                stdin=subprocess.DEVNULL,
                timeout=30
            )

        with open(tmp_path, "r") as infile:
            output = infile.read()
        print(output)
        os.remove(tmp_path)
        return output
    except subprocess.TimeoutExpired:
        os.remove(tmp_path)
        return "TIMEOUT"

def parse_output(output):
    if output == "TIMEOUT":
        return {"error": "timeout"}

    # Extract RESULT line
    result_line = next((line for line in output.splitlines() if line.startswith("RESULT,")), None)
    if not result_line:
        return {"error": "no_result"}

    # Extract statistics (numbers after keywords, e.g. Resumptions: 210)
    stats = {}
    stats_patterns = {
        "resumptions": r"Resumptions:\s*(\d+)",
        "entailments": r"Entailments:\s*(\d+)",
        "prunings": r"Prunings:\s*(\d+)",
        "backtracks": r"Backtracks:\s*(\d+)",
        "constraints_created": r"Constraints created:\s*(\d+)"
    }
    for key, pattern in stats_patterns.items():
        match = re.search(pattern, output)
        stats[key] = int(match.group(1)) if match else 0

    # Extract fields from RESULT line: key=value pairs separated by commas
    result_fields = dict(re.findall(r'(\w+)=([\w\[\]\(\)_\-\+\.]+)', result_line))

    try:
        return {
            "labeling": result_fields.get("labeling", ""),
            "utility": int(result_fields.get("utility", 0)),
            "skills": int(result_fields.get("skills", 0)),
            "preferences": int(result_fields.get("prefs", 0)),
            "experience": int(result_fields.get("expdiv", 0)),
            "time_ms": int(result_fields.get("time_ms", 0)),
            "resumptions": stats["resumptions"],
            "entailments": stats["entailments"],
            "prunings": stats["prunings"],
            "backtracks": stats["backtracks"],
            "constraints_created": stats["constraints_created"],
            "error": ""
        }
    except Exception as e:
        return {"error": f"parse_error: {str(e)}"}

def main():
    os.makedirs(os.path.dirname(RESULTS_FILE), exist_ok=True)

    with open(RESULTS_FILE, "w", newline="") as csvfile:
        fieldnames = [
            "var_order", "value_selection", "value_order",
            "labeling", "utility", "skills", "preferences", "experience",
            "time_ms", "resumptions", "entailments", "prunings", "backtracks", "constraints_created",
            "error"
        ]
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        csvfile.flush()  # Flush after writing header

        for var, val in product(VAR_ORDERINGS, VALUE_SELECTIONS):
            orders = [None] if val.startswith("value(") else VALUE_ORDERINGS

            for order in orders:
                print(f"▶️  Running with variable={var}, value={val}, order={order or 'n/a'}")
                labeling_str = build_labeling(var, val, order)
                output = run_prolog(labeling_str)
                parsed = parse_output(output)

                parsed["var_order"] = var
                parsed["value_selection"] = val
                parsed["value_order"] = order or ""
                parsed["labeling"] = labeling_str

                writer.writerow(parsed)
                csvfile.flush()  # 💾 Ensure line is written to disk immediately

    print(f"\n✅ Done! Results saved to {RESULTS_FILE}")

if __name__ == "__main__":
    main()
