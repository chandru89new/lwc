const storageKeys = {
  publicHolidays: 'phs',
  weekends: 'wknds',
  numberOfForcedLeaves: 'nofl',
  startOfWeek: 'sow'
}
const saveToStorage = (key) => val => {
  return localStorage.setItem(key, val)
}
const getItemWithDefault = defaultValue => key => {
  if (localStorage.getItem(key)) {
    return JSON.parse(localStorage.getItem(key))
  }
  return defaultValue
}
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
    initPublicHolidays: getItemWithDefault(defaultPH)(storageKeys.publicHolidays),
    numberOfForcedLeaves: getItemWithDefault(1)(storageKeys.numberOfForcedLeaves),
    weekendDays: getItemWithDefault(["Sun", "Sat"])(storageKeys.weekends),
    startOfWeek: getItemWithDefault("Sun")(storageKeys.startOfWeek)
  }
});
app.ports.savePhToStorage.subscribe(val => saveToStorage(storageKeys.publicHolidays)(val))
app.ports.saveNumberOfForcedLeavesToStorage.subscribe(val => saveToStorage(storageKeys.numberOfForcedLeaves)(val))


app.ports.saveWeekendDaysToStorage.subscribe(val => saveToStorage(storageKeys.weekends)(val))
app.ports.saveStartOfWeekToStorage.subscribe(val => saveToStorage(storageKeys.startOfWeek)(val))