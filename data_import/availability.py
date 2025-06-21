import re
import requests
import json
import os
from datetime import datetime

# The base date and time to calculate relative time blocks from
BASE_DATETIME = datetime(2024, 9, 14, 0, 0, 0)

LETTUCE_MEET_EVENT_ID = 'eGLXv'

def fetch_lettuce_meet_availability(event_id):
    """
    Fetches availability data from the Lettuce Meet API for a specific event.

    Args:
        event_id (str): The ID for the Lettuce Meet event.

    Returns:
        str: A multi-line string containing the availability data formatted
             as Prolog facts, or an empty string if the request fails.
    """
    print(f"Fetching availability data for event: {event_id}...")
    headers = {
        'authority': 'api.lettucemeet.com',
        'accept': '*/*',
        'accept-language': 'en-US,en;q=0.9',
        'content-type': 'application/json',
        'dnt': '1',
        'origin': 'https://lettucemeet.com',
        'referer': 'https://lettucemeet.com/',
        'sec-ch-ua-mobile': '?0',
        'sec-fetch-dest': 'empty',
        'sec-fetch-mode': 'cors',
        'sec-fetch-site': 'same-site',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36',
    }
    json_data = {
        'id': 'EventQuery',
        'query': 'query EventQuery(\n  $id: ID!\n) {\n  event(id: $id) {\n    ...Event_event\n    id\n  }\n}\n\nfragment Event_event on Event {\n  id\n  pollResponses {\n    user {\n      __typename\n      ... on AnonymousUser {\n        name\n      }\n      ... on User {\n        name\n      }\n    }\n    availabilities {\n      start\n      end\n    }\n  }\n}',
        'variables': {'id': event_id},
    }
    
    try:
        response = requests.post('https://api.lettucemeet.com/graphql', headers=headers, json=json_data)
        response.raise_for_status()  # Raise an exception for bad status codes
        response_json = response.json()
        
        prolog_predicates = []
        jsondata = response_json.get("data", {}).get("event", {}).get("pollResponses", [])

        for i in jsondata:
            # Capitalize the first letter of each word in the name
            user_name = i.get("user", {}).get("name", "Unknown").title()
            
            if "availabilities" in i and i["availabilities"]:
                intervals_for_user = []
                for availability in i["availabilities"]:
                    start_dt = datetime.fromisoformat(availability["start"].replace('Z', ''))
                    end_dt = datetime.fromisoformat(availability["end"].replace('Z', ''))
                    
                    start_block = int((start_dt - BASE_DATETIME).total_seconds() / (15 * 60))
                    end_block = int((end_dt - BASE_DATETIME).total_seconds() / (15 * 60))
                    
                    intervals_for_user.append(f"{start_block}-{end_block}")
                
                if intervals_for_user:
                    prolog_intervals = ", ".join(intervals_for_user)
                    prolog_predicates.append(f"availability('{user_name}', [{prolog_intervals}]).")
        
        print("Successfully fetched and processed data.")
        return "\n".join(prolog_predicates)

    except requests.exceptions.RequestException as e:
        print(f"Error fetching data from Lettuce Meet API: {e}")
        return ""
    except (KeyError, TypeError) as e:
        print(f"Error parsing API response: {e}")
        return ""


def parse_availability(data):
    """
    Parses the availability data strings into a dictionary.
    """
    staff_availability = {}
    pattern = re.compile(r"availability\('([^']*)',\s*\[(.*)\]\)\.")
    for line in data.strip().split('\n'):
        match = pattern.match(line)
        if match:
            name, intervals_str = match.groups()
            intervals = []
            if intervals_str:
                interval_pairs = re.findall(r'(\d+)-(\d+)', intervals_str)
                for start, end in interval_pairs:
                    intervals.append([int(start), int(end)])
            staff_availability[name] = intervals
    return staff_availability

def parse_activities(data):
    """
    Parses the activity data strings into a list of dictionaries.
    """
    activities = []
    pattern = re.compile(r"activity\(([^,]+),\s*\d+,\s*(\d+),\s*(\d+),.*\)\.")
    for line in data.strip().split('\n'):
        match = pattern.match(line)
        if match:
            slug, start_time, duration = match.groups()
            start_time = int(start_time)
            duration = int(duration)
            activities.append({
                'slug': slug.strip(),
                'start': start_time,
                'end': start_time + duration
            })
    return activities

def check_availability(staff_intervals, activity):
    """
    Checks if a staff member is available for a given activity.
    """
    for interval in staff_intervals:
        if interval[0] <= activity['start'] and interval[1] >= activity['end']:
            return 1
    return 0

def generate_prolog_predicate(staff_data, activity_data):
    """
    Generates the full Prolog availability/1 predicate as a string.
    """
    staff_names = sorted(staff_data.keys())
    output_lines = ["availability(["]
    for i, name in enumerate(staff_names):
        intervals = staff_data.get(name, [])
        row = [check_availability(intervals, act) for act in activity_data]
        row_str = f"    [{', '.join(map(str, row))}]"
        comment = f"  % {name}"
        if i < len(staff_names) - 1:
            row_str += ','
        output_lines.append(f"{row_str}{comment}")
    output_lines.append("]).")
    return "\n".join(output_lines)

def generate_json_output(staff_data, activity_data):
    """
    Generates the availability data as a list of dictionaries for JSON output.

    Args:
        staff_data (dict): The parsed staff availability data.
        activity_data (list): The parsed activity data.

    Returns:
        list: A list of dictionaries, ready for JSON serialization.
    """
    staff_names = sorted(staff_data.keys())
    activity_slugs = [act['slug'] for act in activity_data]
    
    json_result = []
    for name in staff_names:
        intervals = staff_data.get(name, [])
        availability_row = [check_availability(intervals, act) for act in activity_data]
        
        staff_entry = {
            "name": name,
            "availability": availability_row,
            "activities": activity_slugs
        }
        json_result.append(staff_entry)
        
    return json_result

# --- Static Activity Data ---
# This data remains hardcoded as it's not part of the API response.
activity_data_str = """
activity(workshop_react_next_1, 2, 40, 8, [react, next, teaching, program]).
activity(escape_room_1, 2, 52, 3, [program]).
activity(workshop_react_next_2, 2, 58, 8, [react, next, teaching, program]).
activity(coffee_break_1, 3, 66, 2, [operations]).
activity(escape_room_2, 2, 68, 3, [program]).
activity(escape_room_3, 2, 71, 3, [program]).
activity(talk_visual_design, 1, 154, 4, [program]).
activity(panel_pi, 1, 164, 4, [program]).
activity(talk_haskell, 1, 168, 2, [program]).
activity(escape_room_4, 2, 148, 3, [program]).
activity(escape_room_5, 2, 151, 3, [program]).
activity(escape_room_6, 2, 154, 3, [program]).
activity(escape_room_7, 2, 157, 3, [program]).
activity(coffee_break_2, 3, 162, 2, [operations]).
activity(talk_chess, 1, 254, 3, [program]).
activity(talk_design_ai, 1, 262, 4, [program]).
activity(workshop_github_actions, 2, 248, 6, [github_actions, teaching, program]).
activity(coffee_break_3, 3, 258, 2, [operations]).
"""

if __name__ == "__main__":
    staff_data_str = fetch_lettuce_meet_availability(LETTUCE_MEET_EVENT_ID)

    if staff_data_str:
        parsed_staff = parse_availability(staff_data_str)
        parsed_activities = parse_activities(activity_data_str)

        prolog_output = generate_prolog_predicate(parsed_staff, parsed_activities)
        json_output = generate_json_output(parsed_staff, parsed_activities)

        os.makedirs('data_pl', exist_ok=True)
        os.makedirs('data_json', exist_ok=True)

        prolog_filepath = 'data_pl/availability.pl'
        with open(prolog_filepath, 'w', encoding='utf-8') as f:
            f.write(prolog_output)
        print(f"\nProlog output successfully written to {prolog_filepath}")

        json_filepath = 'data_json/availability.json'
        with open(json_filepath, 'w', encoding='utf-8') as f:
            json.dump(json_output, f, indent=4, ensure_ascii=False)
        print(f"JSON output successfully written to {json_filepath}")
        
    else:
        print("Could not generate output files due to an error fetching data.")
