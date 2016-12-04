'use strict';



// Require index.html so it gets copied to dist
require('./index.html');

const Elm = require('./Main.elm');

const storedState = localStorage.getItem('elm-todo-save');
const startingState = storedState ? JSON.parse(storedState) : null;

/*
const brain = Elm.Main.fullscreen(startingState);

brain.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('elm-todo-save', JSON.stringify(state));
});
*/

const brain = Elm.Main.fullscreen();

// The third value on embed are the initial values for incomming ports into Elm
//const app = Elm.Main.embed(mountNode);
