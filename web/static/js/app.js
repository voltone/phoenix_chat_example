import {Socket, LongPoll} from "deps/phoenix/web/static/js/phoenix"

window.Phoenix = window.Phoenix || {};
window.Phoenix.Socket = window.Phoenix.Socket || Socket;
window.Phoenix.LongPoll = window.Phoenix.LongPoll || LongPoll;

let chatApp = Elm.embed(
  Elm.PhoenixChat,
  document.getElementById("elm-main"),
  {}
);
