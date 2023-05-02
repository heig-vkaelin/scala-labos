document.addEventListener('DOMContentLoaded', function(){
    const socket = new WebSocket("ws://" + location.host + "/subscribe");
    // when a new message is received remplace the content of the div 
    //   with the id "boardMessage" with the content of the message
    socket.onmessage = function(ev){ boardMessage.innerHTML = ev.data }
}, false);

/**
 * Send a POST with a JSON payload
 * If there is no error replace the input with an empty string
 * If there is an error replace the content of the errorDiv with error message
 */
function submitMessageForm(){
    fetch(
        "/send", 
        {
            method: "POST",
            body: JSON.stringify({msg: messageInput.value})
        }
    ).then(response => response.json())
    .then(json => {
        if(!json.err) {
            messageInput.value = ""
        }
        errorDiv.innerText = json.err
    })
}
