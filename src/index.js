'use strict';

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var storedState = localStorage.getItem('elm-todo-save');
var startingState = storedState ? JSON.parse(storedState) : null;
var brain = Elm.Main.fullscreen(startingState);
brain.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('elm-todo-save', JSON.stringify(state));
});
console.log(brain);
brain.ports.drawCanvas.subscribe(function(data) {
  console.log(data);
});
// The third value on embed are the initial values for incomming ports into Elm
//var app = Elm.Main.embed(mountNode);
