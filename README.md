
# Kaya: A Go Game Analysis Package

**Kaya** is an R package for analyzing and visualizing Go (Baduk/Weiqi) game data. It provides a set of functions to work with Go game records, calculate game statistics, plot game boards, and create animated GIFs to visualize game progression.

## Features

- Parse SGF Files: Read and parse SGF (Smart Game Format) files to extract game data.
- Validate SGF Files: Validate the integrity of SGF files and check for invalid moves or formats.
- Game Moves Analysis: Analyze and update the status of game moves, including grouping stones and checking for illegal moves.
- Liberty Calculation: Count the liberties of stone groups on the game board.
- Board Visualization: Plot Go game boards with stones at specific positions.
- Animated GIF Creation: Generate animated GIFs to visualize the progression of a Go game.
- Customizable Board Size: Supports standard board sizes (9x9, 13x13, 19x19) and custom board sizes up to 52x52.

## Installation

To install the Kaya package, you can use the `devtools` package in R:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install Kaya from GitHub
devtools::install_github("username/Kaya")
```

Replace "username" with the actual GitHub username or repository where the package is hosted.

## Usage

Once installed, you can load the Kaya package in R:

```R
library(kaya)
```

### Parse SGF Files

```R
# Load a sample SGF file
sgf_text <- "(;GM[1]FF[4]CA[UTF-8]AP[Kaya:0.1.0]SZ[19]PW[White]PB[Black];B[dd];W[dp];B[pd];W[qd];B[pp])"

# Parse the SGF file
parsed_game <- parse_sgf(sgf_text)

# Print the parsed game data
print(parsed_game)
```

### Validate SGF Files

```R
# Validate a single SGF file
validate_sgfs("game.sgf")

# Validate multiple SGF files
validate_sgfs(c("game1.sgf", "game2.sgf", "game3.sgf"))
```

### Game Moves Analysis

```R
# Update the status of game moves
game_moves <- read_sgf("game.sgf")
updated_status <- update_status(game_moves)
```

### Liberty Calculation

```R
# Calculate liberties for stone groups
liberties <- count_liberties(game_moves)
```

### Board Visualization

```R
# Plot the game board with stones up to a specific move
plot_game(game_object, number = TRUE, stop = 10)
```

### Animated GIF Creation

```R
# Generate an animated GIF of the game progression
write_gif(game_object, "game_animation.gif", number = TRUE, delay = 50, n_loops = 0, start = 1, stop = 10)
```

## Author and Maintainer

Kaya is developed and maintained by Bret Beheim.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contribution

Contributions are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request on the GitHub repository.

## Acknowledgments

- Thanks to the Go community for their valuable feedback and support.
- Special thanks to the developers of the `igraph` and `animation` packages for enabling advanced graph analysis and GIF creation.

## Contact

For questions or inquiries, you can contact Bret Beheim at bret@example.com.

---

*Please note that this README file is an example and may need modification based on the actual implementation and details of the Kaya package.*