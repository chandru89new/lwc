# long-weekend calculator

Shows a list of possible long-weekends based on some params.

### To run the project:

Because it's such a small project, it's already built by the time it's pushed to this repo. So you can "just run it".

- git clone
- serve the project root so the `index.html` file is served.
That's it.

### To play with the project:

You will need `yarn` (so a node setup) to play around. This is because I use TailwindCSS for the styles and it is processed through PostCSS. These things, however, are abstracted out so you just have to run the script files mentioned below.

These are all terminal commands to be run from the terminal (OSX and Linux shell. For Windows, you might want to use something that emulates shell)

- git clone this repo
- run `sudo chmod +x init` from terminal in this project root. 
- then run `./init` (This will initialize the project. Will download tailwindcss, postcss, uglify-js etc and do one quick build of the project)
- run `sudo chmod +x watcher`
- then run `/.watcher`
- then serve `/index.html` using some live-server (VS Code live server, maybe)
- start modifying the files in `src/` to play around the codebase. The watcher will automatically compile your changes and the live server will refresh the browser serving the `index.html` file.

##### Watching CSS changes as well:

If you want to watch css file changes as well, then you'll have to run these commands in two shells:

- shell 1: `yarn watch-css`
- shell 2: `./watcher`

Or alternatively, `yarn watch-css & ./watcher` from the same shell but I found that if I accidentally type something in the shell after running that command, the `yarn watch-css` job gets suspended. I haven't figured out why (and my shell scripting chops are almost non-existent)

#### To build for "production":

Not that this is important when you play around with the code but if you want to "build" the project, there is a `build` script to run.

- `sudo chmod +x build` to make build executable
- then: `./build`

That's it. This will build a minified js and css.
