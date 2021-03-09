import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


const playSound = (message) => {
  console.log('playing sound', message)
  // TODO
  // const audioContext = new window.AudioContext();
  // const c5Frequency = 523.251130601197269;
};

app.ports.playSound.subscribe((message) => {
  playSound(message);
});