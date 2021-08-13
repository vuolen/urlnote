import { encode, decode } from "./URLNote/Main.purs"

document.getElementById("output").value = window.location.href;

document.getElementById("input").addEventListener("input", (ev) => {
    window.location.hash = encode(ev.target.value);
    document.getElementById("output").value = window.location.href;
});

const updateTextArea = () => {
    document.getElementById("input").value = decode(window.location.hash.substr(1));
}

window.addEventListener("load", updateTextArea);
window.addEventListener("hashchange", updateTextArea);