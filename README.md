# Data Import

Based, by default, on data in `/data_csv` folder. Data already included for the reduced dataset in the shape of `.pl` files in the `/data_pl` folder, and MiniZinc data in the `/minizinc/data` for both dataset instances.

```shell
python3 scripts/data_import/activities_to_json.py reduced_sinf24.csv
python3 scripts/data_import/activities_to_pl.py activities.json
python3 scripts/data_import/staff_to_json.py reduced_staff_forms.csv
python3 scripts/data_import/staff_to_pl.py staff.json
python3 scripts/data_import/availability.py
```

# Experiments

```shell
python3 scripts/run_experiments.py
```

# SICStus

```prolog
consult('sicstus/main.pl').
main. % To customize strategies, use main_stats/1
``` 

# MiniZinc

- Open the MiniZinc IDE

- Load the .mzn model and the desired .dzn data file

- Choose a solver from the dropdown

- Click “Run” to solve the problem and view results