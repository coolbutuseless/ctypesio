
# ctypesio 0.1.1.9001

* [9000] Post CRAN acceptance update
* [9000] Fix for UTF8 read
* [9001] Fix EOF check in `read_str_raw()`.  check number of raw bytes read, 
  not number of characters in string.

# ctypesio 0.1.1  2024-10-10

* Remove C code for integer64 handling. Instead, just treat as `double` 
  when reading/wrting from file and then add/remove `integer64` class []
* Updated UTF-8 handling (PR #1)
* Add "drop = FALSE" when manipulating arrays with `aperm_*()`
* Tighten up error/test when writing hex as integer
* Append to raw vector when given as a connection when writing
* Bugfix for `aperm_array_to_vector()` to have sane ordering

# ctypesio 0.1.0

* Initial release
