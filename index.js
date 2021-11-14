const defaultPH = [
  "2021-01-26",
  "2021-10-02",
  "2021-12-25",
  "2021-08-15"
]
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    year: new Date().getFullYear(),
    initPublicHolidays: JSON.parse(window.localStorage.getItem('phs')) || defaultPH
  }
});
app.ports.saveToStorage.subscribe(val => window.localStorage.setItem('phs', val))