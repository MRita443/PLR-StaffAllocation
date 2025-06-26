import csv
from pathlib import Path
import argparse
from typing import Dict, Any, List, Callable

from utilities import (
    DEFAULT_INPUT_FOLDER,
    DEFAULT_JSON_FOLDER,
    BLOCKS_PER_DAY,
    time_to_block,
    duration_to_blocks,
    export_json_data,
    validate_file_exists,
    ensure_folder_exists,
)


# -------------------------
# Parsers
# -------------------------
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
            "start_time": absolute_start_block,
            "duration": duration_blocks,
            "skills": skills,
        }
    except KeyError as e:
        raise ValueError(f"Missing column in CSV: {e}")
    except ValueError as e:
        raise ValueError(f"Invalid value in row {row}: {e}")


# -------------------------
# Conversion Logic
# -------------------------
def convert_csv_to_json(
    input_path: Path,
    json_path: Path,
    parser: Callable[[Dict[str, str]], Dict[str, Any]],
    top_level_json_key: str,
) -> None:
    """
    Reads data from a CSV file, parses it, and exports it to a JSON file.
    """
    validate_file_exists(input_path)
    ensure_folder_exists(json_path.parent)

    parsed_data = []

    with input_path.open(newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for i, row in enumerate(reader, start=2):
            try:
                parsed = parser(row)
                parsed_data.append(parsed)
            except Exception as e:
                raise ValueError(f"Error on CSV row {i}: {e}\nRow data: {row}")

    export_json_data(parsed_data, json_path, top_level_json_key)


# -------------------------
# Entry Point
# -------------------------
def main() -> None:
    parser = argparse.ArgumentParser(description="Convert CSV data to JSON format.")
    parser.add_argument("input_file", help="Input CSV filename (required)")
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_INPUT_FOLDER,
        type=Path,
        help=f"Input folder (default: {DEFAULT_INPUT_FOLDER})",
    )
    parser.add_argument(
        "-j",
        "--json-folder",
        default=DEFAULT_JSON_FOLDER,
        type=Path,
        help=f"Output folder for JSON (default: {DEFAULT_JSON_FOLDER})",
    )
    parser.add_argument(
        "-J",
        "--json-file",
        default="activities.json",
        help="Output JSON filename (default: activities.json)",
    )
    parser.add_argument(
        "--json-key",
        default="activity",
        help="Top-level key for the JSON output (default: 'activity')",
    )

    args = parser.parse_args()

    input_path = args.input_folder / args.input_file
    json_path = args.json_folder / args.json_file

    try:
        convert_csv_to_json(input_path, json_path, parse_activity_row, args.json_key)
        print(f"✔ Successfully generated JSON: {json_path}")
    except Exception as e:
        print(f"❌ Error: {e}")


if __name__ == "__main__":
    main()