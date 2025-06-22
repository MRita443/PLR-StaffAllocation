import json
import logging
from pathlib import Path
import argparse
from typing import Dict, Any, List
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


def format_staff_member_to_prolog(staff_member: Dict[str, Any]) -> str:
    """
    Formats a single staff member's data into Prolog facts.
    Includes staff details and preferences, omitting preference facts for null values.
    """
    staff_id_atom = prolog_atom(staff_member["name"])
    experience = staff_member["experience"]

    # staff(+name, +experience, +skills)
    skills_list_str = (
        "[" + ", ".join(prolog_atom(skill) for skill in staff_member["skills"]) + "]"
    )
    staff_fact = f"staff({staff_id_atom}, {experience}, {skills_list_str}).\n"

    # preference(staffName, activitySlug, preference) -> [0, 5] integer values
    # Omit preference facts if the value is null
    preference_facts = []
    for activity_slug, rating in staff_member["preferences"].items():
        if rating is not None:  # Only include if rating is NOT null
            activity_atom = prolog_atom(activity_slug)
            preference_facts.append(
                f"preference({staff_id_atom}, {activity_atom}, {rating}).\n"
            )

    # Combine facts for this staff member
    return (
        staff_fact + "".join(sorted(preference_facts)) + "\n"
    )  # Add extra newline for separation between staff


# --- Conversion ---
def convert_json_to_prolog(
    input_json_path: Path,
    output_prolog_path: Path,
) -> None:
    """
    Reads staff data from a JSON file and converts it into Prolog facts,
    writing them to an output .pl file.
    """
    validate_file_exists(input_json_path)  # Now correctly passes input_json_path
    ensure_folder_exists(output_prolog_path.parent)

    with input_json_path.open(encoding="utf-8") as json_file:
        data = json.load(json_file)

    staff_members: List[Dict[str, Any]] = data.get("staff", [])

    if not staff_members:
        logger.warning(f"No staff data found in JSON file: {input_json_path.resolve()}")
        with output_prolog_path.open("w", encoding="utf-8") as out:
            out.write("% No staff data found to convert.\n")
        return

    # Define the module and its exported predicates
    module_header = ":- module(staff, [staff/3, preference/3]).\n\n"

    with output_prolog_path.open("w", encoding="utf-8") as out:
        out.write(module_header)
        out.write("% ############## Staff Data ##############\n\n")

        for staff_member in staff_members:
            prolog_facts = format_staff_member_to_prolog(staff_member)
            out.write(prolog_facts)

    logger.info(
        f"✔ Successfully converted staff JSON to Prolog: {output_prolog_path.resolve()}"
    )


# --- CLI Entry Point ---
def main() -> None:
    """Command-line interface entry point for converting staff JSON to Prolog."""
    parser = argparse.ArgumentParser(
        description="Convert staff JSON data to Prolog facts."
    )
    parser.add_argument(
        "input_file", help="Input staff JSON filename (e.g., 'staff.json')"
    )
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_JSON_FOLDER,
        type=Path,
        help=f"Input folder for JSON (default: '{DEFAULT_JSON_FOLDER.name}')",
    )
    parser.add_argument(
        "-o",
        "--output-folder",
        default=DEFAULT_OUTPUT_FOLDER,
        type=Path,
        help=f"Output folder for Prolog facts (default: '{DEFAULT_OUTPUT_FOLDER.name}')",
    )
    parser.add_argument(
        "-O",
        "--output-filename",
        default="staff.pl",
        help="Output Prolog filename (default: 'staff.pl')",
    )

    args = parser.parse_args()

    input_path = args.input_folder / args.input_file
    output_path = args.output_folder / args.output_filename

    try:
        convert_json_to_prolog(input_path, output_path)
    except FileNotFoundError as e:
        logger.error(f"❌ File error: {e}")
    except json.JSONDecodeError as e:
        logger.error(f"❌ JSON decoding error. Ensure input JSON is well-formed: {e}")
    except Exception as e:
        logger.error(f"❌ An unexpected error occurred: {e}", exc_info=True)


if __name__ == "__main__":
    main()
