# ctypesio 0.1.0.9004

* [9000] Remove C code for integer64 handling. Instead, just treat as `double` 
  when reading/wrting from file and then add/remove `integer64` class []
* [9001] Updated UTF-8 handling (PR #1)
* [9002] Add "drop = FALSE" when manipulating arrays with `aperm_*()`
* [9003] Tighten up error/test when writing hex as integer
* [9004] Append to raw vector when given as a connection when writing
* [9005] Bugfix for `aperm_array_to_vector()` to have sane ordering

# ctypesio 0.1.0

* Initial release
