# long-weekend calculator

Shows a list of possible long-weekends based on some params.

### To run the project:
- git clone
- serve the project root so the `index.html` file is served

### To play with the project:
- git clone
- run `sudo chmod +x watcher` from terminal on the root folder
- then in terminal: `/.watcher/`
- serve `/index.html` using some live-server (vs code live server, maybe)
- start modifying the files in src/ to play around the codebase.

The watcher will automatically compile your changes and the live server will refresh the browser serving the `index.html` file.

### To build for "production":

Not that this is important when you play around with the code but if you want to "build" the project, there is a `build` script to run. You will need `uglify-js` (and therefore `node` + `npm`) to run the build command.
Once you have `node` and `npm` installed:

- in terminal: `sudo npm i -g uglify-js`
- `sudo chmod +x build` to make build executable
- then in terminal: `./build`

That's it.




