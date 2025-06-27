import csv
import json
import unicodedata
import re
import logging
from pathlib import Path
import argparse
from typing import Dict, Any, List, Optional, Tuple, Set
from utilities import (
    DEFAULT_JSON_FOLDER,
    DEFAULT_INPUT_FOLDER,
    validate_file_exists,
    ensure_folder_exists,
    export_json_data
)

# --- Logger Setup ---
logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")


# --- Utilities ---
def normalize_slug(text: str) -> str:
    """Normalizes text to a URL-friendly slug format."""
    text = unicodedata.normalize("NFD", text).encode("ascii", "ignore").decode("utf-8")
    return re.sub(r"\s+", "_", text.strip().lower())


# --- Slug Extraction & Validation ---
def extract_event_slugs_and_indices(
    headers: List[str], valid_activity_slugs: Optional[List[str]] = None
) -> Tuple[List[Tuple[str, int]], List[str]]:
    """
    Extracts event slugs and their column indices from CSV headers.
    Maps general CSV headers to specific activity slugs.
    Returns a list of tuples (specific_valid_slug, column_index) and a list of
    original normalized base slugs found in the headers (for initial preference mapping).
    """
    mapped_header_info: List[Tuple[str, int]] = []
    original_header_base_slugs: List[str] = (
        []
    )  # Stores normalized base slug from header

    valid_slugs_set = set(valid_activity_slugs) if valid_activity_slugs else set()

    # Group valid activity slugs by their base slug for efficient lookup
    base_to_specific_valid_slugs: Dict[str, List[str]] = {}
    for slug in valid_slugs_set:
        base = re.sub(r"_\d+$", "", slug)
        base_to_specific_valid_slugs.setdefault(base, []).append(slug)

    # Sort specific slugs to ensure consistent order if multiple exist for one base
    for base in base_to_specific_valid_slugs:
        base_to_specific_valid_slugs[base].sort()

    # Start from index 5 as the first 5 columns are fixed staff data
    for i, header in enumerate(headers[5:], start=5):
        match = re.search(r"\[(.*?)\]", header)
        if not match:
            continue

        extracted_name = match.group(1)
        normalized_base_slug = normalize_slug(extracted_name)

        # We need to track the base slug from the header for initial preference setting
        original_header_base_slugs.append(normalized_base_slug)

        target_slug_for_header: Optional[str] = None

        if normalized_base_slug in valid_slugs_set:
            # Direct match (e.g., header [workshop_react_next_1] and valid slug workshop_react_next_1)
            target_slug_for_header = normalized_base_slug
        elif normalized_base_slug in base_to_specific_valid_slugs:
            # Base slug match (e.g., header [Coffee Break] and valid slugs coffee_break_1, coffee_break_2)
            # Pick the first specific slug in sorted order to map this column to
            target_slug_for_header = base_to_specific_valid_slugs[normalized_base_slug][
                0
            ]

        if target_slug_for_header:
            mapped_header_info.append((target_slug_for_header, i))
        else:
            logger.warning(
                f"Skipping column '{header}' (normalized: '{normalized_base_slug}') "
                f"as it doesn't match any valid activity slug or its base."
            )

    return mapped_header_info, original_header_base_slugs


# --- Preference Propagation ---
def propagate_preferences(
    preferences: Dict[str, Optional[int]], valid_slugs: List[str]
) -> None:
    """
    Propagates preferences from base slugs or existing specific slugs to other related
    specific slugs within the 'valid_slugs' list if their preference is None.
    """
    valid_slugs_set = set(valid_slugs)

    # Store explicit preferences that are for valid slugs
    explicit_prefs: Dict[str, int] = {
        slug: pref
        for slug, pref in preferences.items()
        if isinstance(pref, int) and slug in valid_slugs_set
    }

    # Group valid slugs by their base (e.g., 'coffee_break' for 'coffee_break_1', 'coffee_break_2')
    base_to_specific_slugs: Dict[str, List[str]] = {}
    for slug in valid_slugs_set:
        base = re.sub(r"_\d+$", "", slug)
        base_to_specific_slugs.setdefault(base, []).append(slug)

    # Sort specific slugs for consistent propagation order
    for base in base_to_specific_slugs:
        base_to_specific_slugs[base].sort()

    for base, specific_slugs_in_group in base_to_specific_slugs.items():
        propagating_value: Optional[int] = None

        # 1. Check if the base slug itself has an explicit preference
        if base in explicit_prefs:
            propagating_value = explicit_prefs[base]

        # 2. If not, check if any specific slug within this group has an explicit preference
        if propagating_value is None:
            for s_slug in specific_slugs_in_group:
                if s_slug in explicit_prefs:
                    propagating_value = explicit_prefs[s_slug]
                    break  # Found a value, no need to check further specific slugs in this group

        # If a propagating value was found, apply it to all specific slugs in the group
        # whose current preference is None
        if isinstance(propagating_value, int):
            for s_slug in specific_slugs_in_group:
                if (
                    preferences.get(s_slug) is None
                ):  # Only fill if current preference is None
                    preferences[s_slug] = propagating_value


# --- Parsing Staff Data ---
def parse_staff_row(
    row: List[str],
    mapped_header_info: List[Tuple[str, int]],
    all_valid_activity_slugs: List[str],
) -> Optional[Dict[str, Any]]:
    """Parses a single row of staff data from the CSV."""
    if len(row) < 5 or not row[2].strip():
        # Skip rows that are too short or have no name
        return None

    name = row[2].strip()
    email = row[1].strip()
    # Safely convert experience to int, defaulting to 0 if not a digit
    experience = int(row[3].strip()) if row[3].strip().isdigit() else 0
    # Normalize skills, filtering out empty strings
    skills = [normalize_slug(s) for s in row[4].split(",") if s.strip()]

    # Initialize preferences for all valid slugs to None
    preferences: Dict[str, Optional[int]] = { slug: None for slug in all_valid_activity_slugs }

    # Populate initial preferences directly from CSV columns using mapped_header_info
    # mapped_header_info provides (target_slug_for_header, column_index)
    for target_slug, col_idx in mapped_header_info:
        if col_idx < len(row):
            val = row[col_idx].strip()
            if val.isdigit():
                preferences[target_slug] = int(val)
            # If val is not digit or column is out of bounds, preference remains None

    # Apply propagation rules to fill in missing preferences
    propagate_preferences(preferences, all_valid_activity_slugs)

    return {
        "name": name,
        "email": email,
        "experience": experience,
        "skills": skills,
        "preferences": preferences,
    }


def parse_staff_csv(
    file_path: Path, valid_activity_slugs: Optional[List[str]] = None
) -> List[Dict[str, Any]]:
    """
    Parses staff data from a CSV file.
    """
    validate_file_exists(file_path)
    staff_data: List[Dict[str, Any]] = []

    with file_path.open(newline="", encoding="utf-8") as f:
        reader = csv.reader(f)
        headers = next(reader)  # Read header row
        # logger.debug(f"CSV Headers: {headers}")

        mapped_header_info, original_header_base_slugs = extract_event_slugs_and_indices(
            headers, valid_activity_slugs
        )

        # Log identified activities for debugging
        if mapped_header_info:
            logger.info(
                f"Identified {len(mapped_header_info)} activity preference columns: "
                f"{[(slug, idx) for slug, idx in mapped_header_info]}"
            )
        else:
            logger.warning("No activity preference columns identified in CSV headers.")

        for i, row in enumerate(reader, start=2):  # Enumerate from 2 for row number
            if not any(row):  # Skip entirely empty rows
                continue
            try:
                staff_member = parse_staff_row(
                    row, mapped_header_info, valid_activity_slugs or []
                )
                if staff_member:
                    staff_data.append(staff_member)
            except Exception as e:
                logger.error(
                    f"Error parsing row {i}: {row}. Error: {e}", exc_info=True
                )
                continue  # Skip this row and continue with the next

    if not staff_data:
        logger.warning(f"No valid staff data parsed from file: {file_path}")

    return staff_data


def get_all_activity_slugs_from_json(json_path: Path) -> List[str]:
    """
    Reads activity slugs from a JSON file (e.g., activities.json).
    Assumes the JSON structure is a dictionary with a key 'activity'
    containing a list of activity dictionaries, each with a 'slug' key.
    """
    try:
        validate_file_exists(json_path)
        with json_path.open(encoding="utf-8") as f:
            data = json.load(f)
        activities = data.get("activity", [])
        slugs = sorted([activity["slug"] for activity in activities if "slug" in activity])
        logger.info(f"Loaded {len(slugs)} activity slugs from {json_path.name}")
        # logger.debug(f"Activity Slugs: {slugs}")
        return slugs
    except FileNotFoundError:
        logger.warning(f"Activities JSON file not found: {json_path}. Cannot validate skills against activities.")
        return []
    except json.JSONDecodeError as e:
        logger.error(f"Error decoding activities JSON from {json_path}: {e}")
        return []
    except KeyError as e:
        logger.error(f"Missing expected key in activities JSON from {json_path}: {e}")
        return []


def process_staff_data(
    input_csv_path: Path, output_json_path: Path, activities_json_path: Optional[Path]
) -> None:
    """
    Orchestrates the process of reading staff CSV, validating against activities,
    and exporting to JSON.
    """
    all_valid_activity_slugs: List[str] = []
    if activities_json_path:
        all_valid_activity_slugs = get_all_activity_slugs_from_json(activities_json_path)
        if not all_valid_activity_slugs:
            logger.warning(
                f"No valid activity slugs found in {activities_json_path}. "
                "Skill and preference validation may be limited."
            )
    else:
        logger.info("No activities JSON path provided. Skipping skill validation against known activities.")


    staff_data = parse_staff_csv(input_csv_path, all_valid_activity_slugs)

    if staff_data:
        ensure_folder_exists(output_json_path.parent)
        export_json_data(
            staff_data, output_json_path, "staff"
        )
        logger.info(
            f"Successfully converted staff data from {input_csv_path.name} to "
            f"JSON in {output_json_path.resolve()}."
        )
    else:
        logger.warning(f"No staff data to write to JSON from {input_csv_path.name}.")


# -------------------------
# Entry Point
# -------------------------
def main() -> None:
    """Main function for importing staff CSV and exporting to JSON."""
    parser = argparse.ArgumentParser(
        description="Convert staff CSV data to JSON format, with optional activity validation."
    )
    parser.add_argument(
        "input_file", help="Input staff CSV filename (e.g., 'staff.csv')"
    )
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_INPUT_FOLDER,
        type=Path,
        help=f"Input folder for CSV (default: '{DEFAULT_INPUT_FOLDER.name}')",
    )
    parser.add_argument(
        "-j",
        "--json-folder",
        default=DEFAULT_JSON_FOLDER,
        type=Path,
        help=f"Output folder for staff JSON (default: '{DEFAULT_JSON_FOLDER.name}')",
    )
    parser.add_argument(
        "-o",
        "--output-filename",
        default="staff.json",  # Renamed default output file
        help="Output JSON filename (default: 'staff.json')",
    )
    parser.add_argument(
        "-a",
        "--activities-json",
        default=f"{DEFAULT_JSON_FOLDER.name}/activities.json",
        type=Path,
        help="Path to activities JSON for skill validation and comprehensive slug checks (optional)",
    )

    args = parser.parse_args()

    input_path = args.input_folder / args.input_file
    json_path = args.json_folder / args.output_filename

    try:
        process_staff_data(input_path, json_path, args.activities_json)
    except FileNotFoundError as e:
        logger.error(f"❌ File error: {e}")
    except json.JSONDecodeError as e:
        logger.error(
            f"❌ JSON decoding error. Ensure activities JSON is well-formed: {e}"
        )
    except Exception as e:
        logger.exception(f"❌ An unexpected error occurred: {e}")


if __name__ == "__main__":
    main()