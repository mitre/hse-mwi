/**
 * Light API to interact with postMessages, including default config
 * to resize iframes.
 *
 * This same library must be loaded in both windows / frames. Any CORS
 * policies must be correctly configured if not loading from the same server.
 *
 * Can be included either directly in `<script>` tags, or as an AMD module.
 *
 * Use on parent:
 * 	Include script, then initialize like:
 *
	SJP = new SJP({
		// the protocol, host, and port of the URL to send messages to
		sendToUrl: http://host-of-the-iframe:port/,

		// the object OR name of window to send messages to
		// resolved by window.frames[name]
		// you can use "parent" to specify this window's parent
		sendToWindow: 'name_of_window', // or document.getElementById('window-id')

		// receive messages from these URLs
		// this will usually be the same as the sendToUrl
		receiveFromUrl: http://host-to-receive-from:port/,

		// don't register handlers for resizing iframes
		// for senders, this means do not send messages onLoad to resize the iframe
		// for receivers, this means do not listen for messages to resize the iframe

		// should this window send events to the sendToUrl when it changes size?
		// if this is a parent window, this should usually be false
		sendResizeEvents: false,

		// should this window receive events from the receiveFromUrl when
		recvResizeEvents: true
	});
 *
 *
 *
 *
 */

  (function () {
    if (typeof define === 'function') {
      define([], function () {
        return SJP;
      })
    } else {
      return window.SJP = SJP;
    }
  
    function SJP(opts) {
      // this is a version number that is checked on both sides of the iframe
      // if any incompatible changes are made, increment it
      var API_VERSION = 1;
  
      var win = window;
  
      /**
       * The URL of the window to post to
       *
       * This is the target origin of the postMessage() call
       */
      var sendToUrl;
  
      /**
       * The window object to postMessage() to.
       *
       * @type {Window}
       */
      var sendToWindow;
  
      /**
       * If passed a name of a window / frame, store here and resolve later
       */
      var sendToWindowName;
  
      /**
       * An array of sender origins from which to handle messages
       *
       * This is the event.origin property when handling received messages
       *
       * @type {String}
       */
      var receiveFromUrl;
  
      /**
       * Automatically set up mutation observers to send messages when content is resized
       *
       * @type {boolean}
       */
      var sendResizeEvents = true;
  
      /**
       * Automatically set up listeners to resize iframes when a message is received
       * @type {boolean}
       */
      var recvResizeEvents = true;
  
      var debug = false;
  
      /********
       * Utils
       */
      var trueFunc = function () {
        return true;
      };
  
      var checkReceivedOrigin = function (origin) {
        return receiveFromUrl === origin;
      };
  
      var isSJPMsg = function (msg, version) {
        version = version || API_VERSION;
  
        if (!msg['SJP_JS_API'] || msg['SJP_JS_API'] != version) {
          return false;
        }
  
        if (!msg['cmd'] || !msg['data']) {
          return false;
        }
  
        return true;
      };
  
      // https://davidwalsh.name/javascript-debounce-function
      var debounce = function (func, wait, immediate) {
        var timeout;
        return function () {
          var context = this, args = arguments;
          var later = function () {
            timeout = null;
            if (!immediate) {
              func.apply(context, args);
            }
          };
          var callNow = immediate && !timeout;
          clearTimeout(timeout);
          timeout = setTimeout(later, wait);
          if (callNow) {
            func.apply(context, args);
          }
        };
      };
  
      var getIframeSize = function () {
        return {
          height: win.document.body.scrollHeight,
          width: win.document.body.scrollWidth
        }
      };
  
      var urlToHost = function(url) {
        var re = /^https?:\/\//;
        var parts = url.split('/');
        var prot = 'https://';
  
        if (re.test(url)) {
          prot = url.split('/')[0] + '//';
          return prot + parts[2];
        } else {
          return prot + parts[0];
        }
      };
  
      var debug = function() {};
  
  
      /***********************
       * Message handling
       * Used on parent pages that include the iframe tags
       ***********************/
      for (var opt in opts) {
        switch (opt) {
          case 'sendToUrl':
            sendToUrl = urlToHost(opts[opt]);
            break;
  
          case 'receiveFromUrl':
            receiveFromUrl = urlToHost(opts[opt]);
            break;
  
          case 'window':
            win = opts[opt];
            break;
  
          case 'sendResizeEvents':
            sendResizeEvents = opts[opt];
            break;
  
          case 'recvResizeEvents':
            recvResizeEvents = opts[opt];
            break;
  
          case 'sendToWindow':
            var tmp = opts[opt];
  
            if (typeof tmp === 'object') {
              debug("sendToWindow is an object. Setting to ", tmp);
              sendToWindow = tmp;
            } else {
              debug("sendToWindow is a string. Deferring resolution until first call to send()", tmp);
              sendToWindowName = tmp;
            }
            break;
  
          case 'debug':
            if (opts[opt]) {
              debug = function() {
                console.log(window.location.toString(), arguments);
              }
            }
            break;
        }
      }
  
      /**
       * Add a handler for receiving a message
       *
       * The handler function should access arguments: data, cmd_name, event
       *
       * @param cmd
       * @param handler
       * @param replace
       */
      var recv = function (cmd, handler, replace) {
        debug("Configuring event handler for cmd", cmd);
        handler = handler || trueFunc;
  
        // @todo add option for replacing event handlers instead of appending
        replace = replace || false;
  
        win.addEventListener("message", function (e) {
          debug("<--- Recv msg", e.data.cmd, e);
  
          if (!checkReceivedOrigin(e.origin)) {
            throw win.location.toString() + ": Discarding message invalid origin of " + e.origin + '. '
            + "Configured to listen for " + receiveFromUrl;
          }
  
          if (!isSJPMsg(e.data)) {
            debug("Ignoring message because it's not a SJP API msg");
            return;
          }
  
          if (e.data.cmd === cmd) {
            debug("Dispatching handler for", cmd, e.data.data);
            handler(e.data.data, cmd, e);
          }
        });
      };
  
      /***********************
       * Message sending
       ***********************/
  
      var makeMsg = function (cmd, data) {
        var r = {
          SJP_JS_API: API_VERSION,
          cmd: cmd,
          data: data || {}
        };
  
        r.data.senderName = win.name;
  
        return r;
      };
  
      var send = function (cmd, data, cb) {
        if (!sendToWindow && sendToWindowName && win.frames[sendToWindowName]) {
          debug("sendToWindowName is found in frames", win.frames[sendToWindowName]);
          sendToWindow = win.frames[sendToWindowName];
        }
  
        var msg = makeMsg(cmd, data);
        cb = cb || trueFunc;
  
        if (!sendToWindow) {
          throw "No window configured to send messages to!";
        }
  
        // don't send messages to ourselves
        if (sendToWindow === win) {
          debug("Not posting message because parent seems to be myself", sendToWindow, win);
          return;
        }
  
        debug("--->", cmd, ' to ', sendToWindow, data, msg);
        sendToWindow.postMessage(msg, sendToUrl);
        return cb();
      };
  
      /*********
       * Iframe resizing
       */
  
      var requestIframeResize = debounce(function (data) {
        debug("Called debounced requestIframeResize()", data);
        send('resize-iframes', data);
      }, 100, false);
  
      if (sendResizeEvents) {
        // set up the observer
        var MutationObserver = win.MutationObserver || win.WebKitMutationObserver;
  
        var observer = new MutationObserver(function (mutations, observer) {
          debug("mutations");
          debug(mutations);
          requestIframeResize(getIframeSize());
        });
  
        var target = document.querySelector('body');
  
        var config = {
          attributes: true,
          attributeOldValue: false,
          characterData: true,
          characterDataOldValue: false,
          childList: true,
          subtree: true
        };
  
        observer.observe(target, config);
  
        // fire off an initial resize request
        debug("initial req");
        requestIframeResize(getIframeSize());
  
        recv('send-initial-size', function() {
          requestIframeResize(getIframeSize());
        });
      }
  
      if (recvResizeEvents) {
        recv('resize-iframes', function (data) {
          var frames = win.document.getElementsByName(data.senderName);
  
          if (frames.length < 1) {
            return;
          }
  
          var frame = frames[0];
          var height = parseInt(data.height + 25) + "px";
  
          debug("Setting iframe", frame, "to " + height);
          frame.style.height = height;
  
          // @todo no width?
        });
  
        // fire an initial request for the size.
        // we do this from the listener side so the iframe
        // doesn't send its initial size before we can receive it
        send('send-initial-size');
      }
  
      /********
       * Default handlers
       */
      recv('reload', function (data) {
        win.location.reload();
      });
  
      recv('flash-message', function (data) {
        var type = data.type || 'info';
        var msg = data.msg;
  
        if (!elgg || !msg) {
          return;
        }
  
        if (type == 'info') {
          elgg.system_message(msg);
        } else {
          elgg.register_error(msg);
        }
      });
  
      return {
        API_VERSION: API_VERSION,
        makeMsg: makeMsg,
        isSJPMsg: isSJPMsg,
        send: send,
        recv: recv,
        requestIframeResize: function () {
          return requestIframeResize(getIframeSize());
        },
        debounce: debounce
      };
    }
  })();
  