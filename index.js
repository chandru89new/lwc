const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    year: new Date().getFullYear(),
    initPublicHolidays: JSON.parse(window.localStorage.getItem('phs')) || []
  }
});
app.ports.saveToStorage.subscribe(val => window.localStorage.setItem('phs', val))