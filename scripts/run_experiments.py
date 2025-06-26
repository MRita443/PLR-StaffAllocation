#!/usr/bin/env python3
"""
Benchmark script for comparing different search strategies in the staff allocation solver.
Runs SICStus Prolog with different search strategies and collects performance metrics.
Updated to use comprehensive labeling options.
"""

import subprocess
import csv
import time
import os
import sys
import tempfile
import re
from itertools import product
from typing import List, Dict, Optional

# --- Configuration ---

# Variable ordering strategies
VAR_ORDERINGS = [
    "leftmost", "ff", "anti_first_fail", "occurrence",
    "ffc", "max_regret", "impact", "dom_w_deg"
]

# Value selection strategies
VALUE_SELECTIONS = [
    "step", "enum", "bisect", "median", "middle", "value(selRandomValue)"
]

# Value orderings (only applies to built-in value selection strategies)
VALUE_ORDERINGS = ["up", "down"]

PROLOG_SOURCE_FILES = [
    'sicstus/main.pl',
    'sicstus/solver.pl',
    'sicstus/optimization.pl',
    'sicstus/constraints.pl',
    'sicstus/data.pl',
    'sicstus/utils.pl'
]

CSV_FIELDNAMES = [
    'strategy',
    'var_order',
    'value_selection',
    'value_order',
    'objective_value',
    'cpu_time_ms',
    'wall_time_s',
    'resumptions',
    'entailments',
    'prunings',
    'backtracks',
    'constraints'
]

DEFAULT_OUTPUT_FILE = 'results/sicstus.csv'
TIMEOUT_SECONDS = 120

# --- Helper Functions ---

def build_labeling_strategy(var_order: str, value_selection: str, value_order: Optional[str] = None) -> List[str]:
    """Builds a labeling strategy as a list of options."""
    if value_selection.startswith("value("):
        return [var_order, value_selection]
    else:
        return [var_order, value_selection, value_order]

def _format_prolog_options(options: List[str]) -> str:
    """Converts a Python list of options to a Prolog list string."""
    return f"[{','.join(options)}]" if options else "[]"

def _parse_prolog_result_line(line: str, strategy_name: str) -> Optional[Dict[str, str]]:
    """
    Parses a single 'RESULT:' line from the Prolog solver's output.
    Expected format: RESULT:options,objective,time,resumptions,entailments,prunings,backtracks,constraints
    """
    if not line.startswith('RESULT:'):
        return None

    content = line[len('RESULT:'):]
    
    # The options part might contain commas and needs careful parsing
    if content.startswith('['):
        bracket_count = 0
        end_options_idx = -1
        for i, char in enumerate(content):
            if char == '[':
                bracket_count += 1
            elif char == ']':
                bracket_count -= 1
                if bracket_count == 0 and i + 1 < len(content) and content[i + 1] == ',':
                    end_options_idx = i + 1
                    break
        
        if end_options_idx == -1:
            print(f"Warning: Invalid result format for options {strategy_name}: {line}")
            return None
        
        options_part = content[:end_options_idx]
        remaining_parts_str = content[end_options_idx + 1:]
        parts = [options_part] + remaining_parts_str.split(',')
    else:
        parts = content.split(',')

    if len(parts) != 8:
        print(f"Warning: Invalid result format for options {strategy_name}: {line} (got {len(parts)} parts, expected 8)")
        return None
    
    return {
        'strategy': strategy_name,
        'objective_value': parts[1],
        'cpu_time_ms': parts[2],
        'resumptions': parts[3],
        'entailments': parts[4],
        'prunings': parts[5],
        'backtracks': parts[6],
        'constraints': parts[7],
    }

def _parse_alternative_result_format(output: str, strategy_name: str) -> Optional[Dict[str, str]]:
    """
    Alternative parser for result lines using key=value format.
    Fallback for when RESULT: format is not found.
    """
    # Extract RESULT line with key=value pairs
    result_line = next((line for line in output.splitlines() if line.startswith("RESULT,")), None)
    if not result_line:
        return None

    # Extract statistics (numbers after keywords)
    stats = {}
    stats_patterns = {
        "resumptions": r"Resumptions:\s*(\d+)",
        "entailments": r"Entailments:\s*(\d+)",
        "prunings": r"Prunings:\s*(\d+)",
        "backtracks": r"Backtracks:\s*(\d+)",
        "constraints": r"Constraints created:\s*(\d+)"
    }
    for key, pattern in stats_patterns.items():
        match = re.search(pattern, output)
        stats[key] = match.group(1) if match else "0"

    # Extract fields from RESULT line: key=value pairs
    result_fields = dict(re.findall(r'(\w+)=([\w\[\]\(\)_\-\+\.]+)', result_line))

    try:
        return {
            'strategy': strategy_name,
            'objective_value': result_fields.get("utility", "0"),
            'cpu_time_ms': result_fields.get("time_ms", "0"),
            'resumptions': stats["resumptions"],
            'entailments': stats["entailments"],
            'prunings': stats["prunings"],
            'backtracks': stats["backtracks"],
            'constraints': stats["constraints"],
        }
    except Exception:
        return None

# --- Core Logic ---

def run_prolog_solver(var_order: str, value_selection: str, value_order: Optional[str] = None) -> Dict[str, str]:
    """
    Runs the Prolog solver with specific labeling options and captures its output.
    Includes timeout support to prevent hanging on difficult problems.
    
    Args:
        var_order: Variable ordering strategy
        value_selection: Value selection strategy  
        value_order: Value ordering (up/down), None for custom value selections
        
    Returns:
        A dictionary containing the parsed results or error information.
    """
    labeling_options = build_labeling_strategy(var_order, value_selection, value_order)
    strategy_name = _format_prolog_options(labeling_options)
    prolog_query = f"main({strategy_name}), halt."
    
    cmd = [
        'sicstus',
        '-l', 'sicstus/main.pl', 
        '--goal', prolog_query,
        '--noinfo' 
    ]
    
    print(f"Running: var={var_order}, val={value_selection}, order={value_order or 'n/a'}")
    print(f"Strategy: {strategy_name}")
    print(f"Command: {' '.join(cmd)}")
    
    # Create temporary file for output capture
    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as tmp_file:
        tmp_path = tmp_file.name

    start_time = time.time()
    try:
        with open(tmp_path, "w") as outfile:
            result = subprocess.run(
                cmd,
                stdout=outfile,
                stderr=outfile,
                stdin=subprocess.DEVNULL,
                timeout=TIMEOUT_SECONDS,
                cwd='.'
            )
        
        wall_time = time.time() - start_time
        
        with open(tmp_path, "r") as infile:
            output = infile.read()
            
        os.remove(tmp_path)
        
        print(f"Exit code: {result.returncode}")
        print(f"Wall clock time: {wall_time:.2f}s")
        
        if output:
            print(f"OUTPUT:\n{output}")
            
        # Try primary parsing method first
        parsed_result = None
        for line in output.splitlines():
            parsed_result = _parse_prolog_result_line(line, strategy_name)
            if parsed_result:
                break
        
        # If primary parsing failed, try alternative format
        if not parsed_result:
            parsed_result = _parse_alternative_result_format(output, strategy_name)
        
        if parsed_result:
            parsed_result['wall_time_s'] = f"{wall_time:.3f}"
            parsed_result['var_order'] = var_order
            parsed_result['value_selection'] = value_selection
            parsed_result['value_order'] = value_order or ""
            return parsed_result
        else:
            print(f"No valid result found in output for strategy {strategy_name}")
            return {
                'strategy': strategy_name, 
                'var_order': var_order,
                'value_selection': value_selection,
                'value_order': value_order or "",
                'objective_value': 'NO_RESULT',
                'cpu_time_ms': '0', 'resumptions': '0', 'entailments': '0',
                'prunings': '0', 'backtracks': '0', 'constraints': '0',
                'wall_time_s': f"{wall_time:.3f}"
            }
            
    except subprocess.TimeoutExpired:
        wall_time = time.time() - start_time
        os.remove(tmp_path)
        print(f"TIMEOUT after {TIMEOUT_SECONDS}s")
        return {
            'strategy': strategy_name,
            'var_order': var_order,
            'value_selection': value_selection, 
            'value_order': value_order or "",
            'objective_value': 'TIMEOUT',
            'cpu_time_ms': '0', 'resumptions': '0', 'entailments': '0',
            'prunings': '0', 'backtracks': '0', 'constraints': '0',
            'wall_time_s': f"{wall_time:.3f}"
        }
        
    except FileNotFoundError:
        print(f"Error: 'sicstus' command not found. Ensure SICStus Prolog is installed and in your PATH.")
        sys.exit(1) # Critical error, exit
    except Exception as e:
        wall_time = time.time() - start_time
        print(f"Error running options {strategy_name}: {e}")
        return {
            'strategy': strategy_name,
            'var_order': var_order,
            'value_selection': value_selection,
            'value_order': value_order or "",
            'objective_value': 'ERROR',
            'cpu_time_ms': '0', 'resumptions': '0', 'entailments': '0',
            'prunings': '0', 'backtracks': '0', 'constraints': '0',
            'wall_time_s': f"{wall_time:.3f}"
        }

def write_results_to_csv(results: List[Dict[str, str]], filename: str) -> None:
    """
    Writes the benchmark results to a CSV file.
    """
    if not results:
        print("No results to write to CSV.")
        return

    os.makedirs(os.path.dirname(filename), exist_ok=True) # Ensure output directory exists
    
    try:
        with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=CSV_FIELDNAMES)
            writer.writeheader()
            writer.writerows(results)
        print(f"\nResults successfully written to {filename}")
    except IOError as e:
        print(f"Error writing CSV file {filename}: {e}")

def write_results_incrementally(filename: str):
    """
    Returns a CSV writer for incremental result writing.
    """
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    csvfile = open(filename, 'w', newline='', encoding='utf-8')
    writer = csv.DictWriter(csvfile, fieldnames=CSV_FIELDNAMES)
    writer.writeheader()
    csvfile.flush()
    return writer, csvfile

def print_summary(results: List[Dict[str, str]]) -> None:
    """
    Prints a formatted summary of the benchmark results.
    """
    print("\n" + "="*100)
    print("COMPREHENSIVE BENCHMARK SUMMARY")
    print("="*100)
    
    successful_runs = [r for r in results if r['objective_value'] not in ['NO_RESULT', 'ERROR', 'TIMEOUT']]
    failed_runs = [r for r in results if r['objective_value'] in ['NO_RESULT', 'ERROR']]
    timeout_runs = [r for r in results if r['objective_value'] == 'TIMEOUT']
    
    print(f"Total strategies tested: {len(results)}")
    print(f"Successful runs: {len(successful_runs)}")
    print(f"Failed runs: {len(failed_runs)}")
    print(f"Timeout runs: {len(timeout_runs)}")
    
    if successful_runs:
        print("\nTop 10 successful runs (ranked by objective value, then by CPU time):")
        # Sort by objective value (descending) then by CPU time (ascending)
        try:
            successful_runs.sort(key=lambda x: (-int(x['objective_value']), int(x['cpu_time_ms'])))
        except ValueError:
            # Fallback to CPU time sorting if objective values aren't numeric
            successful_runs.sort(key=lambda x: int(x['cpu_time_ms']))
        
        # Print header
        print(f"{'#':<3} {'Var Order':<15} {'Value Sel':<20} {'Order':<6} {'Obj':>8} {'CPU(ms)':>8} {'Wall(s)':>8} {'Backtracks':>10}")
        print(f"{'-'*3:<3} {'-'*15:<15} {'-'*20:<20} {'-'*6:<6} {'-'*8:>8} {'-'*8:>8} {'-'*8:>8} {'-'*10:>10}")

        for i, result in enumerate(successful_runs[:10], 1):
            print(f"{i:<3d} {result['var_order']:<15.15} {result['value_selection']:<20.20} "
                  f"{result['value_order']:<6.6} {result['objective_value']:>8} {result['cpu_time_ms']:>8} "
                  f"{result['wall_time_s']:>8} {result['backtracks']:>10}")
    
    if failed_runs or timeout_runs:
        print(f"\nFailed/Timeout runs:")
        if failed_runs:
            print(f"  Failed: {len(failed_runs)} runs")
        if timeout_runs:
            print(f"  Timeout: {len(timeout_runs)} runs (>{TIMEOUT_SECONDS}s each)")

def check_prerequisites() -> bool:
    """
    Checks if all necessary prerequisites (SICStus Prolog, source files) are available.
    """
    print("--- Checking Prerequisites ---")
    
    # Check for SICStus Prolog executable
    try:
        # Use a short timeout for the version check to avoid hanging
        subprocess.run(['sicstus', '--version'], capture_output=True, text=True, check=True, timeout=10)
        print("✓ SICStus Prolog found in PATH.")
    except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
        print("✗ Error: SICStus Prolog not found or not working properly.")
        print("Please ensure SICStus Prolog is installed and its executable is in your system's PATH.")
        return False
        
    # Check for required Prolog source files
    missing_files = [f for f in PROLOG_SOURCE_FILES if not os.path.exists(f)]
    
    if missing_files:
        print("✗ Error: Missing required Prolog source files:")
        for file in missing_files:
            print(f"  - {file}")
        print("Please ensure the 'sicstus/' directory and its contents are present.")
        return False
    else:
        print("✓ All required Prolog source files found.")
        
    print("------------------------------")
    return True

# --- Main Execution ---

def main():
    """Main function to orchestrate the benchmarking process."""
    print("Staff Allocation Solver - Comprehensive Labeling Options Benchmark")
    print("=" * 80)
    
    if not check_prerequisites():
        sys.exit(1)
        
    output_filename = DEFAULT_OUTPUT_FILE
    
    # Generate all combinations
    all_combinations = []
    for var, val in product(VAR_ORDERINGS, VALUE_SELECTIONS):
        if val.startswith("value("):
            # Custom value selection doesn't use ordering
            all_combinations.append((var, val, None))
        else:
            # Built-in value selection uses both orderings
            for order in VALUE_ORDERINGS:
                all_combinations.append((var, val, order))
    
    total_combinations = len(all_combinations)
        
    print(f"\n--- Benchmark Configuration ---")
    print(f"Variable orderings: {len(VAR_ORDERINGS)}")
    print(f"Value selections: {len(VALUE_SELECTIONS)}")
    print(f"Value orderings: {len(VALUE_ORDERINGS)}")
    print(f"Total combinations to test: {total_combinations}")
    print(f"Timeout per run: {TIMEOUT_SECONDS}s")
    print(f"Results will be saved to: {output_filename}")
    print(f"-------------------------------")
    
    # Setup incremental CSV writing
    writer, csvfile = write_results_incrementally(output_filename)
    all_results: List[Dict[str, str]] = []
    
    try:
        for i, (var_order, value_selection, value_order) in enumerate(all_combinations, 1):
            print(f"\n--- Running Test {i}/{total_combinations} ---")
            result = run_prolog_solver(var_order, value_selection, value_order)
            if result:
                all_results.append(result)
                writer.writerow(result)
                csvfile.flush()  # Ensure data is written immediately
                
                status_indicator = "✓ Success" if result['objective_value'] not in ['NO_RESULT', 'ERROR', 'TIMEOUT'] else f"✗ {result['objective_value']}"
                print(f"{status_indicator}: Objective = {result['objective_value']}, "
                      f"CPU Time = {result['cpu_time_ms']}ms, "
                      f"Wall Time = {result['wall_time_s']}s")
            else:
                print("✗ Failed to retrieve benchmark result for this strategy.")
    
    finally:
        csvfile.close()
            
    if all_results:
        print_summary(all_results)
        print(f"\n✅ Benchmark completed! Results saved to {output_filename}")
    else:
        print("\nNo benchmark results were collected!")
        sys.exit(1)

if __name__ == '__main__':
    main()
