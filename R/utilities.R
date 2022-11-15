## Utility functions -- used internally in the package
## NOT available to users of the package


## Test to see if a path is absolute or relative:
## found this at: https://github.com/r-lib/fs/issues/22

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}
