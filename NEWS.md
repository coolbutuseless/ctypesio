# ctypesio 0.1.1

* Remove C code for integer64 handling. Instead, just treat as `double` 
  when reading/wrting from file and then add/remove `integer64` class []
* Updated UTF-8 handling (PR #1)
* Add "drop = FALSE" when manipulating arrays with `aperm_*()`
* Tighten up error/test when writing hex as integer
* Append to raw vector when given as a connection when writing
* Bugfix for `aperm_array_to_vector()` to have sane ordering

# ctypesio 0.1.0

* Initial release
