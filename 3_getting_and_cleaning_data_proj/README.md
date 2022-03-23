### Getting and Cleaning Data Course Project

If you have the ZIP file in the working directory, run
```
run_analysis()
```

If you have a data directory which contains the ZIP file, run
```
run_analysis(destdir="path/to/data/dir")
```

If you want to download the file, run
```
run_analysis(destdir="path/to/data/dir", should_download=TRUE)
```

The script will create two files in the data directory: `smartphones-tidy.txt` and `smartphones-summary.txt`
`smartphones-tidy.txt` is the tidy dataset coming from the raw data.
`smartphones-summary.txt` is the summary dataset. It has the averages all columns in the tidy dataset by `subject` and `activity`

Check out `codebook.txt` to learn about the variables used in the tidy and summary datasets.