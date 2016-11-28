'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');
import { drawCanvases } from './canvas';

const Elm = require('./Main.elm');
const mountNode = document.getElementById('main');

const storedState = localStorage.getItem('elm-todo-save');
const startingState = storedState ? JSON.parse(storedState) : null;

/*
const brain = Elm.Main.fullscreen(startingState);

brain.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('elm-todo-save', JSON.stringify(state));
});
*/

const brain = Elm.Main.fullscreen();
brain.ports.canvasMessage.subscribe(function(data) {
  drawCanvases(data.payload);
});
// The third value on embed are the initial values for incomming ports into Elm
//const app = Elm.Main.embed(mountNode);
