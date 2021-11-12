const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    year: new Date().getFullYear()
  }
})