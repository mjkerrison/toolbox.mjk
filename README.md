
# mjkerrison's toolbox

A monorepository for functions and snippets that:

- Are too useful not to save and reuse
- Are refined too often to be copied-and-pasted around
- Sometimes need to be shared with others
- But don't quite feel like they should be turned into a full package that has to be rebuilt and reinstalled repeatedly

# How best to use this

Check out https://mjkerrison.com/R+and+Data/Design+patterns#import+from+%22loose%22+scripts - roughly:

- Use {pins} to retrieve the code itself
- Use {import} (like `import::from(...)`) to load the code
  - Though of course, if you *are* saving the code locally anyway, one could just `source(...)` it all.

# Other attempts

- Doing weird stuff with like a .Rprofile script
  - See, e.g., `~/exploration/.Rprofile`
  - I think in general this is best avoided - becomes too idiosyncratic for good data science / reproducibility etc.

# To do

- [ ] Clean up `~/snippets` - break them down to 1 per file for better composability
- [ ] Clean up `~/R` - also break stuff down to 1 per file
