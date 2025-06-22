# json_to_prolog.py
import json
import logging
from pathlib import Path
import argparse
from typing import Dict, Any, List, Callable

from utilities import (
    DEFAULT_JSON_FOLDER,
    DEFAULT_OUTPUT_FOLDER,
    prolog_atom,
    validate_file_exists,
    ensure_folder_exists,
)

# --- Logger Setup ---
logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


# -------------------------
# Prolog Formatters
# -------------------------
def format_activity_pl(activity: Dict[str, Any]) -> str:
    skills_str = (
        "[" + ", ".join(prolog_atom(skill) for skill in activity["skills"]) + "]"
    )
    slug = prolog_atom(activity["slug"])
    return (
        f"activity({slug}, {activity['min_staff']}, "
        f"{activity['start_time']}, {activity['duration']}, {skills_str}).\n"
    )


# -------------------------
# Conversion Logic
# -------------------------
def convert_json_to_prolog(
    input_json_path: Path,
    output_prolog_path: Path,
    formatter: Callable[[Dict[str, Any]], str],
    top_level_json_key: str,
    module_name: str,
    module_predicates: List[str],
) -> None:
    """
    Reads data from a JSON file and converts it into Prolog facts,
    writing them to an output .pl file.
    """
    validate_file_exists(input_json_path)
    ensure_folder_exists(output_prolog_path.parent)

    with output_prolog_path.open("w", encoding="utf-8") as out:
        # Define the module and its exported predicates
        predicates_str = ", ".join(module_predicates)
        module_header = f":- module({module_name}, [{predicates_str}]).\n\n"
        out.write(module_header)
        out.write(
            f"% ############## {top_level_json_key.capitalize()} Data ##############\n\n"
        )

        with input_json_path.open(encoding="utf-8") as json_file:
            data = json.load(json_file)

        items_to_convert: List[Dict[str, Any]] = data.get(top_level_json_key, [])

        if not items_to_convert:
            logger.warning(
                f"No data found under key '{top_level_json_key}' in JSON file: {input_json_path.resolve()}"
            )
            # No need to open again, just write the comment
            out.write(f"% No {top_level_json_key} data found to convert.\n")
            return  # Exit function if no items

        for i, item in enumerate(items_to_convert, start=1):
            try:
                prolog_fact = formatter(item)
                out.write(prolog_fact)
            except Exception as e:
                raise ValueError(
                    f"Error converting item {i} to Prolog: {e}\nItem data: {item}"
                )

    logger.info(
        f"✔ Successfully converted JSON to Prolog: {output_prolog_path.resolve()}"
    )


# -------------------------
# Entry Point
# -------------------------
def main() -> None:
    parser = argparse.ArgumentParser(description="Convert JSON data to Prolog facts.")
    parser.add_argument("input_file", help="Input JSON filename (required)")
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_JSON_FOLDER,
        type=Path,
        help=f"Input folder for JSON (default: {DEFAULT_JSON_FOLDER})",
    )
    parser.add_argument(
        "-o",
        "--output-folder",
        default=DEFAULT_OUTPUT_FOLDER,
        type=Path,
        help=f"Output folder for Prolog (default: {DEFAULT_OUTPUT_FOLDER})",
    )
    parser.add_argument(
        "-O",
        "--output-file",
        default="activities.pl",
        help="Output Prolog filename (default: activities.pl)",
    )
    parser.add_argument(
        "--json-key",
        default="activity",
        help="Top-level key in the JSON file containing the list of items (default: 'activity')",
    )
    parser.add_argument(
        "--module-name",
        default="activities",
        help="Name of the Prolog module (default: 'activities')",
    )
    parser.add_argument(
        "--module-predicates",
        nargs="+",
        default=["activity/5"],
        help="List of predicates to export in the Prolog module (default: 'activity/5')",
    )

    args = parser.parse_args()

    input_path = args.input_folder / args.input_file
    output_path = args.output_folder / args.output_file

    try:
        convert_json_to_prolog(
            input_path,
            output_path,
            format_activity_pl,  # This is hardcoded for activity, could be generalized
            args.json_key,
            args.module_name,
            args.module_predicates,
        )
    except FileNotFoundError as e:
        logger.error(f"❌ File error: {e}")
    except json.JSONDecodeError as e:
        logger.error(f"❌ JSON decoding error. Ensure input JSON is well-formed: {e}")
    except Exception as e:
        logger.error(f"❌ An unexpected error occurred: {e}", exc_info=True)


if __name__ == "__main__":
    main()
