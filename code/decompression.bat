@echo off
set source_dir=../data/zip_data
set dest_dir=../data/raw_data

if not exist "%dest_dir%" (
    mkdir "%dest_dir%"
)

for %%f in ("%source_dir%\*.zip") do (
    tar -xf "%%f" -C "%dest_dir%"
)

echo Unzipping completed!
pause
