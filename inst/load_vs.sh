#!/usr/bin/env bash

# create ./utils folder under ./src/Python if doesn't exist
if [ ! -d "./src/Python/utils" ]; then
    mkdir -p "./src/Python/utils"
fi

# create __init__.py if it doesn't exist
if [ ! -f "./src/Python/utils/__init__.py" ]; then
    echo "# enable custom function import from utils folder" > "./src/Python/utils/__init__.py"
fi

# fetch customized util files from ./phecdm repo
UTIL_FILES=("gen_vs_json_utils")
for f in "${UTIL_FILES[@]}"; do
    SRC_URL="https://raw.githubusercontent.com/RWD2E/phecdm/refs/heads/main/src/${f}.py"
    DEST_PATH="./src/Python/utils/${f}.py"
    echo "${f}.py"
    curl -L "$SRC_URL" -o "$DEST_PATH"
    echo "from .${f} import *" >> "./src/Python/utils/__init__.py"
done

# fetch customized json ref files
REF_FILES=(
    "vs-cde-dm"
    "vs-cde-als"
)
for r in "${REF_FILES[@]}"; do
    SRC_URL="https://raw.githubusercontent.com/RWD2E/phecdm/refs/heads/main/res/valueset_curated/${r}.json"
    DEST_PATH="./ref/${r}.json"
    echo "${r}.json"
    curl -L "$SRC_URL" -o "$DEST_PATH"
done

# chomd +x ./inst/load_vs.sh
# ./inst/load_vs.sh