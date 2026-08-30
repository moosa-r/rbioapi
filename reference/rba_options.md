# Set rbioapi Global Options

A safe way to change rbioapi's global options and behavior. see
"arguments" section for available options.  
Note that you are not limited to changing the options globally, you can
include the option names and values in the '...' argument of any rbioapi
function to alter the option(s) only in that function call; e.g.
example_function(x, diagnostics = TRUE, timeout = 300).  
Alternatively, you can call this function with no arguments, i.e.
rba_options(), to retrieve a data frame of available rbioapi options and
their current values.

## Usage

``` r
rba_options(
  diagnostics = NULL,
  dir_name = NULL,
  retry_max = NULL,
  retry_wait = NULL,
  progress = NULL,
  save_file = NULL,
  skip_error = NULL,
  timeout = NULL,
  verbose = NULL,
  metadata = NULL
)
```

## Arguments

- diagnostics:

  Logical: (optional) Show diagnostics and detailed messages with
  internal information. The package default is `FALSE`.

- dir_name:

  Character: (optional) If the package needs to generate a file path to
  save the server's response, a directory with this name will be created
  in your working directory to save your files. The package default is
  `"rbioapi"`.

- retry_max:

  Numeric: (optional) How many times should rbioapi retry in case of 5xx
  server responses, errors related to the server or no internet
  connectivity? Must be a finite non-negative whole number. The package
  default is `0`.

- retry_wait:

  Numeric: (optional) Time in seconds to wait before next retry in case
  of internet connection or server problems. Must be finite and
  non-negative. The package default is `10`.

- progress:

  Logical: (optional) Should a progress bar be displayed? The package
  default is `FALSE`.

- save_file:

  Logical: (optional) Either:

  - TRUE: In this case, the raw server's response file will be
    automatically saved to a proper file path. use "dir_name" argument
    to change the file's parent directory.

  - FALSE: Do not automatically save server's response file.

  - Character: (Only when changing the option via "..." in a functions
    call) A valid file path to save the server's response file to the
    function that you are calling.

  The package default is `FALSE`.

- skip_error:

  Logical: (optional) If TRUE, the code execution will not be stopped in
  case of errors (anything but HTTP status 200 from the server); Instead
  the error message will be returned as the function's output. However,
  if FALSE, in case of any error, the code execution will be halted and
  an error message will be issued. The package default is `FALSE` in
  interactive sessions and `TRUE` otherwise.

- timeout:

  Numeric: (optional) The maximum time in seconds that you are willing
  to wait for a server response before giving up and stopping the
  function execution. Accepted values are between 0.001 and 3600,
  inclusive. The package default is `90`.

- verbose:

  Logical: (optional) Generate short informative messages. The package
  default is `TRUE`.

- metadata:

  Logical: (optional) Save API request metadata with returned objects?
  It includes the rbioapi version and, for each request, the timestamp,
  API call, original `httr` response, and exact parser functions. Use
  [`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md)
  to get it. The package default is `FALSE`.

## Value

If called without any argument, a Data frame with available options and
their information; If Called with an argument, will Return NULL but
Alters that option globally.

## Details

Because this function validates your supplied changes, please ***only
change rbioapi options using this function*** and avoid directly editing
them.

## See also

Other "Helper functions":
[`rba_connection_test()`](https://rbioapi.moosa-r.com/reference/rba_connection_test.md),
[`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md),
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md)

## Examples

``` r
rba_options()
if (FALSE) { # \dontrun{
rba_options(verbose = FALSE)
} # }
if (FALSE) { # \dontrun{
rba_options(save_file = TRUE)
} # }
if (FALSE) { # \dontrun{
rba_options(diagnostics = TRUE, progress = TRUE)
} # }
if (FALSE) { # \dontrun{
## Save metadata with all later rbioapi calls:
rba_options(metadata = TRUE)

## Turn it off again:
rba_options(metadata = FALSE)
} # }
```
