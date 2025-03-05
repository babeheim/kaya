
# v2.7.0 (2025-03-05)

- 

# v2.6.5 (2022-10-04) [20f33e1](https://github.com/babeheim/kaya/commit/20f33e1)
- removed the base-R pipe (`|>`) to work on older R versions

# v2.6.4 (2022-08-30) [2ebe795](https://github.com/babeheim/kaya/commit/2ebe795)
- rewrote `orient_sgf` with new logic; updated game plotting to use negative coordinates; now all valid SGF board sizes allowed
- added a `coord_sgf_original` column if rotation is applied
- updated `orient_sgf` function to include diagonals and axes
- new function: `apply_orientations()`, which applies all transformations at once
- new function: `generate_board_sectors()` which assigns sector numbers to board positions
- fixed small partial match issue

# v2.6.2 (2021-02-25) [40a891d](https://github.com/babeheim/kaya/commit/40a891d)
- fixed a test
- added a `create_database` function
- added `seq_along`
- added a few helper features
- added some more tests
- update_status() now better tracks suicide and illegal moves
- count_liberties() has been streamlined

# v2.6.1 (2020-08-26) [1113408](https://github.com/babeheim/kaya/commit/1113408)
- removed {yamltools} dependency, and included a validation check in `read_sgf`

# v2.6.0 (2018-06-09) [490ad7c](https://github.com/babeheim/kaya/commit/490ad7c)

- more efficient group ID handling with `id_maker()`.
`sapply_pb()` and `lapply_pb()` improved batch processing
- `validate_sgf()` enables faster debugging of faulty SGFs

## Game Playback & Visualization Improvements

- more refined GIF generation (`write_tiny_gif()`).
- `write_sgf()` now handles multi-property metadata better.
- grafted all my new version of `read_sgf` and its dependencies into the package, removing the older versions. The files read, but `simplify_game` doesn't work properly nor has it been integrated into read_sgf, so all the downstream operations fail. Gotta fix `simplify_game`!

# v2.5.4 (2018-05-02) [580fde6](https://github.com/babeheim/kaya/commit/580fde6)
- worked on remaining bug with how unescaped parentheses work, fix was especially simple as I just switched two lines of code with each other in `parse_sgf`
- added explicit ability to load multiple games from one file
- finished off the encountered bugs folder
- closing in on all unit tets passing

# v2.5.3 (2018-04-29) [3b89c85](https://github.com/babeheim/kaya/commit/3b89c85)
- revised the square bracket parsing to fix the iron giant problem, but in the process created many more bugs to fix!
- added unit test for previous bugfix
- fixed bug with `purge_comments`; I was incorrectly selecting any tag ending with `C\[`, not just comment tags. So I added a negative lookbehind.
- added a few more games to `encountered_bugs`

# v2.5.2 (2018-04-28) [8eef2d2](https://github.com/babeheim/kaya/commit/8eef2d2)
- fixed segfault error by creating `purge_comments` function; the problem was there were too many comments
- many bugs fixed from encountering data in the wild
- passing all unit tests! also moved comments into a vignette, needs to be cleaned up
- replaced `parse_sgf` with new version, but still chasing a few bugs having to do with multiple games in the file
- working on a new `parse_sgf` that will totally rock
- added `split_tag` function and reorganized R files a bit; still working on a better `parse_sgf`

# v2.5.1 (2018-04-27) [3204b1e](https://github.com/babeheim/kaya/commit/3204b1e)
- fixed a few bugs, setting up a revision of parse and split functions
- more bugfixes
- modified the regex statement in `comment_pattern`, almost fixing iron giant problem but not every scenario
- a few more tests, identifying but not solving the 'iron giant' problem with escaping brackets
- added a `create_database` function and fixed that parsing problem with inner square brackets that need to be escaped

# v2.5.0 (2018-04-23) [9d94df0](https://github.com/babeheim/kaya/commit/9d94df0)

## SGF Parsing now outputs JSON
- sgf_to_json() converts SGF files into structured JSON.
- parse_sgf() now handles multiple game trees properly.
- Whitespace normalization (scrub_whitespace()) improves compatibility.


- added `sgf_to_json` function
- resolved bug in how passes are handled in `update_status`
- major bugfixes and parsing script restructure
- major refactor of `read_sgf` to allow multiple branches

# v2.4.0 (2018-03-18) [35bcfce](https://github.com/babeheim/kaya/commit/35bcfce)

## Game State Tracking & Validation
- `plot_game()` now tracks and updates game state dynamically.
- incorporated {igraph} package into new version of `id_groups` which is much faster, and re-enabled suicide detection in `update_status` because I now trust the algorithm
- new function: `update_status()` tracks stone groups, captures, and suicide moves.
- `validate_games()` ensures SGF correctness before processing
- Liberties calculation (`count_liberties()`) has been added.
- Group identification (`id_groups()`) ensures stones are properly linked.

- fixed typo in DESCRIPTION
- added new gif writer for cleaner animated diagrams

- fixed bug in the handicap move coding
- linted the codebase
- added tests for status functions, though they are not entirely passing currently
- more edits to vignettes
- reorganized functions into seperate files
- updated package_overview vignette
- revised `plot_game` with removal rules, some bugfixes
- added testthat functionality and added some vignettes, still incomplete

# v2.3.0 (2017-10-28) [c8e6067](https://github.com/babeheim/kaya/commit/c8e6067)

- SGF parsing handles comments, branching variations, and multi-game files better.
- added ability to read chinese characters in player names
- added a `write_kifu()` function
- added a `write_gif()` function, but stone size is weird
- added description
- added `plot_game`
- reorganized how `parse_sgf` outputs game moves, eliminating sgfs letter coordinate system
- added an empty sgf condition to `read_sgf`, though it should be expanded to any invalid sgf, no?

# v2.2.0 (2017-10-26) [9c7e0b4](https://github.com/babeheim/kaya/commit/9c7e0b4)
- added `write_sgf`, allowing saved game reconstruction

# v2.1.1 (2017-10-23) [b65b078](https://github.com/babeheim/kaya/commit/b65b078)
- added man page

# v2.1.0 (2017-10-22) [0877bdb](https://github.com/babeheim/kaya/commit/0877bdb)
- added `orient_sgf` to ensure games are always transformed to a canonical orientation before hashing, which ensures that identical games receive the same hash
- test revision
- added `plot_board` function
- added orientation function with a few tests

# v2.0.2 (2017-10-07) [d49f5e5](https://github.com/babeheim/kaya/commit/d49f5e5)
- one more modification, so games with `)(` do not get parsed incorrectly
- added a few more tweaks
- added more unit test, I feel like I've exhausted all tests except for forking games

# v2.0.1 (2017-10-06) [635a694](https://github.com/babeheim/kaya/commit/635a694)
- added move comments to parser
- minor changes to parser
- added more files to test
- removed multiple inputs on `read_sgf`
- fixed bug with characters outside games, added test

# v2.0.0 (2017-10-05) [1d04477](https://github.com/babeheim/kaya/commit/1d04477)

Cleaner structure
Improved SGF parsing
Better hash-based game identification
Progress-tracking batch processing

new dependencies: {digest}, {stringi}
- `parse_sgf()` can handle multiple games per file
- first commit using formal version control
- restructured {kaya} codebase as a legit R package
- total refactor of codebase using regular expressions, starting anew with three functions:
  - `parse_sgf` uses new splitting logic for identifying tags and values and SHA-1 digest-based hashing replaces `hash.maker()`
  - `read_sgf` reads an SGF file and parses it using `parse_sgf()`
  - `extract_sgf_tag` extracts SGF metadata tags and cleans encoding issues
- added some new tests, capacity to read files containing multiple games
- added first unusual sgf, containing no moves



# v1.2.0 (2013-04-05)

tag: EHS paper

- version associated with the publication "Strategic social learning and the population dynamics of human behavior: the game of Go", by Bret Beheim, Calvin Thigpen, and Richard McElreath, in Evolution and Human Behavior, Volume 35, Issue 5, pp.351-357 DOI: http://dx.doi.org/10.1016/j.evolhumbehav.2014.04.001

1. standardized function naming to `snake_case`
2. smarter SGF parsing
3. more idiomatic approach to functions with `read_sgf()`, `plot_board()`, etc.
4. better graphical handling, including X11 and Quartz support
5. more robust duplicate detection
6. better pattern searching

## specific updates

- `plot_board()` replaces `board.maker()` and handles platform-specific graphical settings (Windows, Linux, Mac)
- `read_sgf()` replaces `game.table.maker()`, loading SGFs from a specified path instead of assuming working directory.
- `sgf.list.to.string()` ensures correct reformatting of handicap stones before output
- `tree.flagger()` checks for tree structures more efficiently
- `pattern.saver()` now returns named list (coordinates, colors)
- `standard.position()` has more efficient coordinate mapping
- `sgf_player()` automatically detects game format (matrix, list, SGF string).
- `nemo_finder()` adds error handling for bounding box mismatches
- `chronography()` now includes opponent ranking data.
- `biography()` formats opponent statistics more clearly



# v1.1.0 - 2012-07-15

1. more robust handling of invalid or unusual SGFs
2. better duplicate game detection using a hash algorithm
3. cleaner and more intelligent visualization functions

## specific updates

- `sgf.string.to.list()` has improved handling of malformed SGF files
- `tag.extractor()`	now checks for multiple left brackets (`[`) before a right bracket (`]`).
- `tree.flagger()` has improved detection of branching variations in SGF files
- `standard.position()`	now uses `sgf.string.to.list()` internally to ensure move order consistency
- `board.maker()`	now adjusts aspect ratio dynamically to maintain board symmetry
- `mirror.mirror()` has improved handling of diagonal reflections
- `game.table.maker()` now skips corrupted SGF files instead of failing the entire run
- `duplicate.detector()` is more efficient at duplicate identification by pre-filtering common hashes
- `nemo.finder()` now supports more advanced bounding box configurations
- `sgf.player()` has improved performance for large game databases
- `kifu.maker()` improved output formatting for PDFs and PNGs
- `pattern.saver()`	now highlights selected stones dynamically during pattern input
- `biography()` now handles missing player records gracefully



# v1.0.0 (2012-06-17)

- tagged version associated with dissertation analysis submitted to registrar on 2012-06-21

1. officially started calling this the "kaya" package
2. totally refactored and streamlined SGF parsing and storage
3. new functions for analyzing trends in games over time
4. improved visualization and search functionality

## specific updates
- `pattern.saver()` now validates bounding box constraints before saving
- `kifu.maker()` now supports plot, PNG, PDF, and PostScript (PS) outputs
- `sgf.player()` automatically detects board size (9x9, 13x13, 19x19)
- `game.table.maker()` now processes SGFs more efficiently
- `standard.position()` now calls `sgf.string.to.list()` internally

## removed functions
- `coordinate.fetcher()`
- `move.extractor()` has been replaced with two modular functions: `sgf.string.to.list()` and `sgf.list.to.string()`
- `raw.sgf.loader()` has been absorbed into `game.table.maker()`
- `string.search()` has been absorbed into `nemo.finder()`

## new functions
- `sgf.string.to.list()` converts an SGF string into a list of moves and colors
- `sgf.list.to.string()` converts an SGF list back to a string format
- `bounding.box.maker()` creates interactive bounding boxes for searches
- `nemo.time()` tracks frequency of moves across historical datasets
- `chronography()` analyzes player career history in a dataset
- `biography()` generates Go career summaries for a player


# v0.4.0 (2011-05-31)

1. substantially improved pattern searching 
2. lots of useful visualization enhancements
3. move-evolution analysis functions

## specific updates
- `tag.extractor()` more robust to missing brackets
- `hash.maker()` ignores handicap stones when computing the hash
- `move.extractor()` detects handicap stones
- `standard.position()` handles handicap stones correctly
- `game.table.maker()` is more robust to game tree structures

## new functions
- `coordinate.fetcher()` converts letter coordinates to numerical grid positions
- `string.search()` replaces `seq.search()`
- `database.player()` plays multiple games in sequence from a database
- `kifu.maker()` generates printable board diagrams (PDF, PNG, PS)
- `click.rectangle()` is a GUI tool for selecting a region on the board
- `string.save()` is a GUI tool for selecting a region on the board
- `board.maker()` now works on both windows and mac 
- `mirror.mirror()` replaces `mirror()`
- `raw.sgf.loader()` loads all SGFs in a path at once with progress bar, replacing `sgf.loader()`
- `sgf.player()` can display 9x9, 13x13, and 19x19 games
- `nemo.finder()` determines *next moves* across a set of games
- `performance.counts()` computes performance statistics for searched patterns
- `player.map()` computes performance statistics for searched patterns
- `player.counts()` computes statistics on first-move players
- `search.saver()` & `search.loader()` saves/loads search results to a CSV file




# v0.3.0 (2011-03-09)

1. better ability to flag duplicate games and detect patterns via standarized game orientations

2. more efficient processing of large numbers of SGF files

3. improved power hash algorithm using modulos

## specific updates
- renamed `tag.ripper()` to `tag.extractor()`, which now checks for missing patterns more intelligently
- `move.extractor` removes extra branches from the SGF
- `standard.position()` computes canonical board orientations
- `hash.maker()` generates unique hash ID's for each game by computes a large sum from the positional encodings of `power.id` but then calculating the modulo to create a short numerical ID
- `sgf.loader()` loads all SGFs at once using a progress bar
- `duplicate.detector()` identifies duplicate games using `hash.maker()`
- `move.table.maker()` is removed
- `sgf.player()` accepts move vectors and single-string SGFs



# v0.2.0 (2011-03-02)

1. adds functions for duplicate game identification

2. major visualization functions: `board.maker()`, `sgf.player()`, and `seq.search()` provide graphical tools for rendering Go boards, replaying games, and searching patterns

3. more robust move extraction that accounts for symmetries with `move.extractor()` and `mirror()` 

## specific updates
- retired `process.games()` to instead use `game.table.maker()`
- `board.maker()` generates a Go board visualization with star points.
- `power.id()` replaces `dyer.sig()` for game identification, converting each move position into a numerical value and computing the sum of each value to the power of 7
- `mirror()` generates all eight board symmetries
- `move.extractor()` - now has `rm.passes=FALSE` argument to remove pass moves

## new functions
- `sapply_pb()` is a progress bar function
- `seq.search()` is a GUI for drawing pattern searches on Go boards
- `sgf.player()` animates SGF move sequences on a Go board
- `sgf.faker()` generates synthetic SGF files for testing
- `angle.binner()` categorizes move angles into bins
- `threedist()` computes geometric distances between move triplets


# v0.1.1 (2011-02-23)

- fixed bug in `locate.games()` related to `grep`
- renamed `ripper()` to `tag.ripper()`
- modularized `process.games()` with new helper functions: `move.extractor()` and `sgf.loader()`

## new functions
- `move.table.maker()` converts SGF move data into a structured table
- `dyer.sig()` is a new function for move-based game signatures, extracting specific moves from an SGF game (e.g., moves at 20, 30, 40) and concatenates them into a single string to use as a fingerprint



# v0.1.0 (2011-02-23)

My initial experiments with SGF processing, using three functions:
- `locate.games()`, a SGF file locator
- `ripper()`, a SGF tag+value extractor
- `process.games()` for game processing, calls `ripper()`



