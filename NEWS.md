
# ctypesio 0.1.2

* Fix for UTF8 reading
* Fix EOF check in `read_str_raw()`.  check number of raw bytes read, 
  not number of characters in string.
* Retain attributes when using a raw vector e.g. endianness setting

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
