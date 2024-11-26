
# wsl2 is required by 
# wsl or wsl.exe exists
wsl.exe --list --oneline

# update to wsl
wsl.exe --install

# navigate to parent folder where the input file is saved
cd C:/repos/cdc_als4m/ref

# download latest docker img
docker run hello-world

# run the base degauss docker
docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder als_mda_coa_degauss_input.csv

# run other dockers for additional geomarkers
docker run --rm -v $PWD:/tmp ghcr.io/degauss-org/census_block_group:0.6.0 als_mda_coa_geocoded.csv
