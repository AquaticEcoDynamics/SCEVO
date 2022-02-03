
<!-- README.md is generated from README.Rmd. Please edit that file -->

# secvo

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Deplying with Docker:

1\) `cd` to the docker file

2\) `docker build -t scevo-dashboard .`

3\) `docker save scevo-dashboard > scevo-dashboard.tar`

4\) Deploy to ARMS via `ssh`

5\) `docker load < scevo-dashboard.tar`

6\) `docker run -d -p 3838:3838 scevo-dashboard`
