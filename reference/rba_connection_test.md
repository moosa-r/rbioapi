# Test if the Supported Services Are Responding

Run this function to test the internet connectivity of your device and
the current status of the supported Services.

## Usage

``` r
rba_connection_test(print_output = TRUE, diagnostics = FALSE)
```

## Arguments

- print_output:

  (Logical) (default = TRUE) Send the tests' output to the console?

- diagnostics:

  (Logical) (default = FALSE) Show diagnostics and detailed messages
  with internal information.

## Value

Connection test for the supported servers will be displayed in console
and the results will be invisibly returned as a list.

## Details

This function attempts to send a simple query to the supported services.
If the service successfully responded, you will be informed with a
success message; If not, the content of the error will be reported to
you.  
Please run this function if you encounter any errors while using
rbioapi. Also, if you need to contact support, kindly call this function
with 'diagnostic = TRUE' and include the output messages in your support
request.

## See also

Other "Helper functions":
[`rba_metadata()`](https://rbioapi.moosa-r.com/reference/rba_metadata.md),
[`rba_options()`](https://rbioapi.moosa-r.com/reference/rba_options.md),
[`rba_pages()`](https://rbioapi.moosa-r.com/reference/rba_pages.md)

## Examples

``` r
# \donttest{
rba_connection_test()
# }
```
