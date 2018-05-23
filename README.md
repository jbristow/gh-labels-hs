# gh-labels-hs-hs

This repo is used to manage the Github issue labels of a given repo

## Prerequisites

* Create a Github personal access token with a scope of `repo` (full control of private repositories)
* Install the [haskell stack](https://haskellstack.org). (On a mac: `brew install haskell-stack`)

## Installation

```
stack install
```

## Usage
The labels that will be created are in `labels.yml`. Any existing labels that
are setup on the repos will be removed if they do not exist in `labels.yml`.
The repos that will be processed exist in organization as defined by `org` in the script.

* Run on a single repo:
    ```bash
    gh-labels-hs -o <repo-owner> -t <gh-token> -r <repo-name>
    ```

* Run the update script in dry-run mode
    ```bash
    gh-labels-hs -o <repo-owner> -t <gh-token> -r <repo-name> --dry-run
    ```
