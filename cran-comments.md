## Test environments
* local OS X install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder, R 2016-07-13 r70908

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking data for non-ASCII characters ... NOTE
  Note: found 1320 marked UTF-8 strings.

The package contains two built-in data.frames hhi_q_i and series_info with names of time series and methodology description in Russian. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
