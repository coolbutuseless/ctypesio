# ctypesio 0.1.0.9003

* [9000] Remove C code for integer64 handling. Instead, just treat as `double` 
  when reading/wrting from file and then add/remove `integer64` class []
* [9001] Updated UTF-8 handling (PR #1)
* [9002] Add "drop = FALSE" when manipulating arrays with `aperm_*()`
* [9003] Tighten up error/test when writing hex as integer

# ctypesio 0.1.0

* Initial release
