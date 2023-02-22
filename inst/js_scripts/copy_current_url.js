function copy_current_url(text) {
       var inputc = document.body.appendChild(document.createElement("input"));
       inputc.value = window.location.href;
       inputc.focus();
       inputc.select();
       document.execCommand("copy");
       inputc.parentNode.removeChild(inputc);
       alert("URL successfully copied.");
}
