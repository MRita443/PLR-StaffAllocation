import csv
from pathlib import Path
import argparse
from typing import Callable, Dict, Any


# -------------------------
# Configs & Constants
# -------------------------
DEFAULT_INPUT_FOLDER = Path("./data_csv")
DEFAULT_OUTPUT_FOLDER = Path("./data_pl")
BLOCK_MINUTES = 15
BLOCKS_PER_DAY = 24 * 60 // BLOCK_MINUTES  # 96 blocks/day


# -------------------------
# Utilities
# -------------------------
def time_to_block(tstr: str) -> int:
    h, m, s = map(int, tstr.strip().split(":"))
    return (h * 60 + m) // BLOCK_MINUTES


def duration_to_blocks(minutes: int) -> int:
    return minutes // BLOCK_MINUTES


def validate_input_file(input_path: Path) -> None:
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path.resolve()}")


def ensure_output_folder(output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)


def parse_activity_row(row: Dict[str, str]) -> Dict[str, Any]:
    """Parse a CSV row into structured activity data (dictionary)."""
    try:
        slug = row["Slug"]
        min_staff = int(row["Min Staff"])
        day = int(row["Day"])
        start_block = time_to_block(row["Start Time"])
        absolute_start_block = (day - 1) * BLOCKS_PER_DAY + start_block
        duration_blocks = duration_to_blocks(int(row["Duration (min)"]))
        skills_field = row["Skills"].strip()
        if skills_field == "":
            skills = []
        else:
            skills = [s.strip() for s in skills_field.split("|") if s.strip()]

        return {
            "slug": slug,
            "min_staff": min_staff,
            "start_block": absolute_start_block,
            "duration_blocks": duration_blocks,
            "skills": skills,
        }
    except KeyError as e:
        raise ValueError(f"Missing column in CSV: {e}")
    except ValueError as e:
        raise ValueError(f"Invalid value in row {row}: {e}")


# -------------------------
# Prolog Formatter
# -------------------------
def prolog_atom(s: str) -> str:
    """
    Convert string to Prolog atom.
    If string is not a valid lowercase identifier, quote and escape it.
    """
    if not s.isidentifier() or not s.islower():
        s_escaped = s.replace("'", "''")  # double single quotes for Prolog atom
        return f"'{s_escaped}'"
    return s


def format_activity_pl(activity: Dict[str, Any]) -> str:
    skills_str = "[" + ", ".join(prolog_atom(skill) for skill in activity["skills"]) + "]"
    slug = prolog_atom(activity["slug"])
    return (
        f"activity({slug}, {activity['min_staff']}, "
        f"{activity['start_block']}, {activity['duration_blocks']}, {skills_str}).\n"
    )


# -------------------------
# Conversion
# -------------------------
def convert_csv_to_prolog(
    input_path: Path,
    output_path: Path,
    parser: Callable[[Dict[str, str]], Dict[str, Any]],
    formatter: Callable[[Dict[str, Any]], str],
) -> None:
    """
    Generic CSV to Prolog fact converter.
    - parser: function that parses a CSV row to a dict or object
    - formatter: function that formats the parsed dict/object to a Prolog fact string
    """
    validate_input_file(input_path)
    ensure_output_folder(output_path)

    with input_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        with output_path.open("w", encoding="utf-8") as out:
            for i, row in enumerate(reader, start=2):
                try:
                    parsed = parser(row)
                    fact = formatter(parsed)
                    out.write(fact)
                except Exception as e:
                    # Improved error message with row context
                    raise ValueError(f"Error on CSV row {i}: {e}\nRow data: {row}")


# -------------------------
# Entry Point
# -------------------------
def main() -> None:
    parser = argparse.ArgumentParser(description="Convert CSV activities to Prolog facts")
    parser.add_argument("input_file", help="Input CSV filename (required)")
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_INPUT_FOLDER,
        type=Path,
        help=f"Input folder (default: {DEFAULT_INPUT_FOLDER})",
    )
    parser.add_argument(
        "-o",
        "--output-folder",
        default=DEFAULT_OUTPUT_FOLDER,
        type=Path,
        help=f"Output folder (default: {DEFAULT_OUTPUT_FOLDER})",
    )
    parser.add_argument(
        "-O", "--output-file", default="activities.pl", help="Output Prolog filename (default: activities.pl)"
    )

    args = parser.parse_args()

    input_path = args.input_folder / args.input_file
    output_path = args.output_folder / args.output_file

    try:
        convert_csv_to_prolog(input_path, output_path, parse_activity_row, format_activity_pl)
        print(f"✔ Successfully generated: {output_path}")
    except Exception as e:
        print(f"❌ Error: {e}")


if __name__ == "__main__":
    main()
