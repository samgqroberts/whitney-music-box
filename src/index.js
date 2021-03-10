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

let audioContext;

// due to web standards, cannot use AudioContext before some user gesture
document.getElementById('startButton').addEventListener('click', () => {
  audioContext = new AudioContext();
})

function parseMessage(message) {
  const node = JSON.parse(message);
  if (!node || typeof node !== 'object') {
    console.error('cannot parse message, given message is not a json object:', message);
    return undefined;
  }
  const frequency = parseFloat(node.frequency);
  if (isNaN(frequency)) {
    console.error('cannot play sound, given frequency is NaN:', message);
    return undefined;
  }
  if (!Array.isArray(node.sineTerms)) {
    console.error('cannot play sound, given sineTerms array is not an array:', message);
    return undefined;
  }
  const sineTerms = node.sineTerms.map(parseFloat);
  if (sineTerms.some(v => isNaN(v))) {
    console.error('cannot play sound, give sineTerms array contains invalid values:', message);
    return undefined;
  }
  return { frequency, sineTerms };
}

const playSound = (message) => {
  if (!audioContext) {
    console.error('cannot play sound, audio context not initialized');
    return;
  }
  const parsedMessage = parseMessage(message);
  if (!parsedMessage) {
    return;
  }
  const { frequency, sineTerms } = parsedMessage;
  const osc = audioContext.createOscillator();
  const gainNode = audioContext.createGain();
  osc.connect(gainNode);
  gainNode.connect(audioContext.destination);
  const cosineTerms = new Float32Array(sineTerms.length);
  const waveform = audioContext.createPeriodicWave(cosineTerms, sineTerms);
  osc.setPeriodicWave(waveform)
  osc.frequency.value = frequency;
  gainNode.gain.setValueAtTime(0.1, audioContext.currentTime);
  gainNode.gain.exponentialRampToValueAtTime(0.0001, audioContext.currentTime + 3.0);
  osc.start();
  setTimeout(() => {
    osc.stop();
  }, 3000);
};

app.ports.playSound.subscribe((message) => {
  playSound(message);
});