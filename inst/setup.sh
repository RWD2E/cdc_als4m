#!/usr/bin/env bash

# This script creates a Python 3.11 virtual environment and installs the Snowflake Snowpark package.

# check if python 3.11 is installed; download the recommended windows installer and install it (checking "Add PATH")
# python --version

# check python 3.11 added to the current PATH
# echo "$PATH" | tr ':' '\n'

# check all Python interpreters in PATH
# which -a python python3 python3.* 2>/dev/null

#use py launcher to toggle between versions (current <-> 3.11)
# py -3.11 --version
# py -3.13 --version

# create a python 3.11 virtual env
# python -m venv py311_venv

# verify python version inside the virtual env
# python --version

# activate venv
# source ./py311_venv/Scripts/activate

# Install dependencies
# note: adding --user at the end will bypass the virtual env and install packages to default location %APPDATA%\Python\PythonXY\site-packages
# pip3 install -r ./inst/requirements.txt

# check packages installed
# python -m site

# check for a specific package
# python -m pip show snowflake-snowpark-python

# install uv
# curl -LsSf https://astral.sh/uv/install.sh | sh

# use uv
uv init -p 3.11
uv --version
uv add snowflake-connector-python
uv add snowflake-snowpark-python
uv add pandas
uv add boto3
uv add openpyxl
