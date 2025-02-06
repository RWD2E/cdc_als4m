# navigate to parent folder where the input file is saved
cd C:/repos/cdc_als4m/ref

# download latest docker img
docker run hello-world

# run the base degauss docker
docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/geocoder TRI_UniqueAddr_DegauseInput.csv all

# run other dockers for additional geomarkers
docker run --rm -v ${PWD}:/tmp ghcr.io/degauss-org/census_block_group:0.6.0 TRI_UniqueAddr_DegauseInput_geocoder_3.4.0_score_threshold_all.csv 2020
