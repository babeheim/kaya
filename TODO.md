

[-] `create_database` should store the source path in `file_path`

[-] look at journal entry 2018-11-13 - Kaya vignettes, and compare with the vignettes/ folder

[-] there has to be a way to incorporate the whole update_status function in write_tiny_gif.R, so we dont have to rerun the same calculations over and over again?

[-] does id_direct_connections handle NA's correctly?

[-] lapply_pb, is this strictly necessary? is my version in longformer better?

[-] what functions are unusued? what aren't being tested?

[-] test_parse_sgf gives it all the different strings i can think of

[-] if 'update_status' expects a group_id, then why doesn't read_sgf assign one? and, does it have to be rando?

[-] i don't think sgf_to_json should be part of kaya itself...more like a batch processing script...

[-] write tests for write_sgf, write then validate!
[-] write tests for write_gif and write_tiny_gif!
[-] what if simplify_game calls update_status and will report if something's wrong!

[x] partial match warning: partial match of 'row' to 'rows'
[x] read_sgf's rotation test...wait! it's simplfy_game's orientation test, called by read_sgf...but did i write the test correctly? 
[x] test simplify_game directly somehow...also its helper functions!
[x] reconcile R's plot coordinates and the SGF coordinate system
[x] validate_sgfs should account for the SIZE of the board in assessing move illegality
[x] orient_sgf: how to handle passes? convert to NA? seperate this step into its own function?
