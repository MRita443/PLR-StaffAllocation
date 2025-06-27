import json
from pathlib import Path
from typing import Any, Dict, List
import logging
import argparse

# -------------------------
# Configs & Constants
# -------------------------
DEFAULT_INPUT_FOLDER = Path("./data_csv")
DEFAULT_OUTPUT_FOLDER = Path("./data_pl")
DEFAULT_JSON_FOLDER = Path("./data_json")

BLOCK_MINUTES = 15
BLOCKS_PER_DAY = 24 * 60 // BLOCK_MINUTES  # 96 blocks/day


# -------------------------
# Utilities
# -------------------------

def create_base_parser(description: str) -> argparse.ArgumentParser:
    """Creates a base ArgumentParser with common input/output folder arguments."""
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument(
        "-i",
        "--input-folder",
        default=DEFAULT_INPUT_FOLDER,
        type=Path,
        help=f"Input folder (default: '{DEFAULT_INPUT_FOLDER.name}')",
    )
    parser.add_argument(
        "-o",
        "--output-folder",
        default=DEFAULT_OUTPUT_FOLDER,
        type=Path,
        help=f"Output folder (default: '{DEFAULT_OUTPUT_FOLDER.name}')",
    )
    return parser

def time_to_block(tstr: str) -> int:
    """Converts a time string (HH:MM:SS) to a block number."""
    h, m, s = map(int, tstr.strip().split(":"))
    return (h * 60 + m) // BLOCK_MINUTES


def duration_to_blocks(minutes: int) -> int:
    """Converts a duration in minutes to a number of blocks."""
    return minutes // BLOCK_MINUTES


def validate_file_exists(file_path: Path) -> None:
    """Validates if the specified file exists."""
    if not file_path.exists():
        raise FileNotFoundError(f"File not found: {file_path.resolve()}")


def ensure_folder_exists(folder_path: Path) -> None:
    """Ensures the specified folder exists, creating it if necessary."""
    folder_path.mkdir(parents=True, exist_ok=True)


def prolog_atom(s: str) -> str:
    """Converts a string into a valid Prolog atom, quoting if necessary."""
    if not s.isidentifier() or not s.islower():
        s_escaped = s.replace("'", "''")
        return f"'{s_escaped}'"
    return s


def export_json_data(
    data_list: List[Dict[str, Any]], json_path: Path, top_level_key: str
) -> None:
    """Exports a list of dictionaries to a JSON file under a specified top-level key."""
    ensure_folder_exists(json_path.parent)
    structured = {top_level_key: data_list}
    with json_path.open("w", encoding="utf-8") as jf:
        json.dump(structured, jf, indent=2, ensure_ascii=False)