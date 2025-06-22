#!/usr/bin/env python3
"""
Benchmark script for comparing different search strategies in the staff allocation solver.
Runs SICStus Prolog with different search strategies and collects performance metrics.
"""

import subprocess
import csv
import time
import os
import sys
from typing import List, Dict, Optional

# --- Configuration ---

# Labeling options to test (maximize(ObjectiveValue) will be appended automatically by the Prolog code)
LABELING_OPTIONS = [
    [],              # Default options
    ['ff'],          # First-fail
    ['ffc'],         # First-fail with choices
    ['min'],         # Minimum value
    ['max'],         # Maximum value
    ['up'],          # Ascending order
    ['down'],        # Descending order
    ['step'],        # Step value selection
    ['enum'],        # Enumeration
    ['bisect'],      # Bisection
    ['ff', 'up'],    # First-fail + ascending
    ['ff', 'down'],  # First-fail + descending
    ['ffc', 'up'],   # First-fail with choices + ascending
    ['ffc', 'down'], # First-fail with choices + descending
]

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
    'objective_value',
    'cpu_time_ms',
    'wall_time_s',
    'resumptions',
    'entailments',
    'prunings',
    'backtracks',
    'constraints'
]

# Hardcoded output filename as arguments are removed
DEFAULT_OUTPUT_FILE = 'results/sicstus.csv'

# --- Helper Functions ---

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

# --- Core Logic ---

def run_prolog_solver(labeling_options: List[str]) -> Dict[str, str]:
    """
    Runs the Prolog solver with specific labeling options and captures its output.
    This version does not include a timeout, so it will wait indefinitely for the solver to complete.
    
    Args:
        labeling_options: A list of strings representing the labeling options.
        
    Returns:
        A dictionary containing the parsed results or error information.
    """
    strategy_name = _format_prolog_options(labeling_options)
    prolog_query = f"main({strategy_name}), halt."
    
    cmd = [
        'sicstus',
        '-l', 'sicstus/main.pl', 
        '--goal', prolog_query,
        '--noinfo' 
    ]
    
    print(f"Running with options: {strategy_name}")
    print(f"Command: {' '.join(cmd)}")
    
    start_time = time.time()
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd='.' # Execute in the current working directory
        )
        wall_time = time.time() - start_time
        
        print(f"Exit code: {result.returncode}")
        print(f"Wall clock time: {wall_time:.2f}s")
        
        if result.stdout:
            print(f"STDOUT:\n{result.stdout}")
        if result.stderr:
            print(f"STDERR:\n{result.stderr}")
            
        parsed_result = None
        for line in result.stdout.splitlines():
            parsed_result = _parse_prolog_result_line(line, strategy_name)
            if parsed_result:
                break
        
        if parsed_result:
            parsed_result['wall_time_s'] = f"{wall_time:.3f}"
            return parsed_result
        else:
            print(f"No 'RESULT:' line found in output for strategy {strategy_name}")
            return {
                'strategy': strategy_name, 'objective_value': 'NO_RESULT',
                'cpu_time_ms': '0', 'resumptions': '0', 'entailments': '0',
                'prunings': '0', 'backtracks': '0', 'constraints': '0',
                'wall_time_s': f"{wall_time:.3f}"
            }
            
    except FileNotFoundError:
        print(f"Error: 'sicstus' command not found. Ensure SICStus Prolog is installed and in your PATH.")
        sys.exit(1) # Critical error, exit
    except Exception as e:
        print(f"Error running options {strategy_name}: {e}")
        return {
            'strategy': strategy_name, 'objective_value': 'ERROR',
            'cpu_time_ms': '0', 'resumptions': '0', 'entailments': '0',
            'prunings': '0', 'backtracks': '0', 'constraints': '0',
            'wall_time_s': '0'
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

def print_summary(results: List[Dict[str, str]]) -> None:
    """
    Prints a formatted summary of the benchmark results.
    """
    print("\n" + "="*80)
    print("BENCHMARK SUMMARY")
    print("="*80)
    
    successful_runs = [r for r in results if r['objective_value'] not in ['NO_RESULT', 'ERROR']]
    failed_runs = [r for r in results if r['objective_value'] in ['NO_RESULT', 'ERROR']]
    
    print(f"Total strategies tested: {len(results)}")
    print(f"Successful runs: {len(successful_runs)}")
    print(f"Failed runs: {len(failed_runs)}")
    
    if successful_runs:
        print("\nSuccessful runs (ranked by CPU time):")
        # Convert CPU time to float for correct sorting
        successful_runs.sort(key=lambda x: float(x['cpu_time_ms']))
        
        # Print header
        print(f"{'#':<3} {'Strategy':<20} {'Obj':<8} {'CPU Time (ms)':>13} {'Wall Time (s)':>15} {'Backtracks':>10}")
        print(f"{'-'*3:<3} {'-'*20:<20} {'-'*8:<8} {'-'*13:>13} {'-'*15:>15} {'-'*10:>10}")

        for i, result in enumerate(successful_runs, 1):
            print(f"{i:<3d} {result['strategy']:<20.20} {result['objective_value']:<8} "
                  f"{result['cpu_time_ms']:>13} {result['wall_time_s']:>15} {result['backtracks']:>10}")
    
    if failed_runs:
        print("\nFailed runs:")
        for result in failed_runs:
            print(f"  {result['strategy']:<20.20} -> {result['objective_value']}")

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
    print("Staff Allocation Solver - Labeling Options Benchmark")
    print("=" * 60)
    
    if not check_prerequisites():
        sys.exit(1)
        
    # No argparse needed, using defaults
    output_filename = DEFAULT_OUTPUT_FILE
    strategies_to_test = LABELING_OPTIONS
        
    print(f"\n--- Benchmark Configuration ---")
    print(f"Number of strategies to test: {len(strategies_to_test)}")
    print(f"Results will be saved to: {output_filename}")
    print(f"-------------------------------")
    
    all_results: List[Dict[str, str]] = []
    
    for i, labeling_options in enumerate(strategies_to_test, 1):
        print(f"\n--- Running Test {i}/{len(strategies_to_test)} ---")
        result = run_prolog_solver(labeling_options)
        if result:
            all_results.append(result)
            status_indicator = "✓ Success" if result['objective_value'] not in ['NO_RESULT', 'ERROR'] else "✗ Failed"
            print(f"{status_indicator}: Objective = {result['objective_value']}, "
                  f"CPU Time = {result['cpu_time_ms']}ms, "
                  f"Wall Time = {result['wall_time_s']}s")
        else:
            print("✗ Failed to retrieve benchmark result for this strategy.")
            
    if all_results:
        write_results_to_csv(all_results, output_filename)
        print_summary(all_results)
    else:
        print("\nNo benchmark results were collected!")
        sys.exit(1)

if __name__ == '__main__':
    main()
