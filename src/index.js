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
let masterGainNode;

// due to web standards, cannot use AudioContext before some user gesture
document.getElementById('startButton').addEventListener('click', () => {
  audioContext = new AudioContext();
  masterGainNode = audioContext.createGain();
  masterGainNode.connect(audioContext.destination);
  masterGainNode.gain.value = 0.1;
})

const playSound = (message) => {
  // TODO use message... at all
  console.log('playing sound', message)
  if (!audioContext) {
    console.error('cannot play sound, audio context not initialized');
    return;
  }
  const osc = audioContext.createOscillator();
  osc.connect(masterGainNode);
  const sineTerms = new Float32Array([0, 0, 1, 0, 1]);
  const cosineTerms = new Float32Array(sineTerms.length);
  const waveform = audioContext.createPeriodicWave(cosineTerms, sineTerms);
  osc.setPeriodicWave(waveform)
  const c5Frequency = 523.251130601197269;
  osc.frequency.value = c5Frequency;
  osc.start();
  setTimeout(() => {
    osc.stop();
  }, 500);
};

app.ports.playSound.subscribe((message) => {
  playSound(message);
});