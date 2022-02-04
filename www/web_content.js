$(document).ready(function () { 

var mwiBaseUrl = "ENTER YOUR URL HERE"; 
//Setup hooks so that SJP can resize the MWI iframe based off the size of the content 
var sjpMsging = new SJP({ 
// the protocol, host, and port of the URL to send messages to 
sendToUrl: mwiBaseUrl, 
// the object OR name of window to send messages to 
// resolved by window.frames[name] 
// you can use "parent" to specify this window's parent 
sendToWindow: window.parent, 
// register a handler to receive messages from these URLs 
receiveFromUrl: mwiBaseUrl, 
sendResizeEvents: true,
recvResizeEvents: false }); 

});
