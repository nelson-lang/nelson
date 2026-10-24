// The uihtml page contract, in one place.
//
// It is served as a static asset from the frontend root, which every window of
// the desktop serves - the docked shell and each separate figure window alike,
// since they share the same root folder. That matters: a figure in its own
// window is a different origin with a different port, so anything served only
// by the desktop window would 404 there.
//
// Both forms of HTMLSource reach this same file. A page given as markup is
// inlined into an iframe with srcdoc and references this absolute path, which
// resolves because an about:srcdoc document inherits the parent's base URL. A
// page given as a file is served by the engine, which injects the same tag.
//
// The page talks to the desktop through postMessage rather than the WebUI
// bridge: it is an iframe of the desktop document, not a window of its own.
(function () {
  'use strict';
  var listeners = {};
  var currentData = null;
  var revision = 0;
  var applying = false;
  var ready = false;

  function post(message) {
    parent.postMessage(message, '*');
  }

  // A page renders its own markup whether or not this bridge ever reached it,
  // so a broken handshake looks exactly like a working one. The badge shows how
  // far the handshake got and removes itself the moment setup() has run, so a
  // healthy page shows nothing and a stuck one says where it stopped.
  var badge = null;
  function status(text) {
    if (!badge) {
      badge = document.createElement('div');
      badge.style.cssText =
        'position:fixed;left:8px;bottom:8px;z-index:2147483647;padding:5px 9px;' +
        'background:#5b1a1a;color:#ffdede;border-radius:4px;' +
        'font:12px/1.4 ui-monospace,monospace';
      if (document.body) {
        document.body.appendChild(badge);
      } else {
        window.addEventListener('DOMContentLoaded', function () {
          document.body.appendChild(badge);
        });
      }
    }
    badge.textContent = 'uihtml: ' + text;
  }
  function clearStatus() {
    if (badge) {
      badge.remove();
      badge = null;
    }
  }

  function dispatch(name, detail) {
    var handlers = listeners[name];
    if (!handlers) {
      return;
    }
    for (var i = 0; i < handlers.length; i++) {
      try {
        handlers[i]({ Source: htmlComponent, Data: detail });
      } catch (err) {
        console.error('uihtml: listener for "' + name + '" failed', err);
      }
    }
  }

  var htmlComponent = {
    get Data() {
      return currentData;
    },
    set Data(value) {
      currentData = value;
      // A value the engine just installed must not be reported straight back.
      if (applying) {
        return;
      }
      post({ __uihtml: 'data', value: value });
    },
    addEventListener: function (name, handler) {
      if (!listeners[name]) {
        listeners[name] = [];
      }
      listeners[name].push(handler);
    },
    removeEventListener: function (name, handler) {
      var handlers = listeners[name];
      if (!handlers) {
        return;
      }
      var index = handlers.indexOf(handler);
      if (index >= 0) {
        handlers.splice(index, 1);
      }
    },
    sendEventToNelson: function (name, data) {
      post({ __uihtml: 'event', name: name, data: data });
    }
  };

  window.addEventListener('message', function (event) {
    var message = event.data;
    if (!message || typeof message !== 'object') {
      return;
    }
    if (message.__uihtml === 'apply') {
      // A stamp that is not newer is an echo of what this page already holds.
      if (message.revision <= revision) {
        return;
      }
      revision = message.revision;
      applying = true;
      try {
        currentData = message.value;
        if (ready) {
          dispatch('DataChanged', currentData);
        }
      } finally {
        applying = false;
      }
    } else if (message.__uihtml === 'event') {
      dispatch(message.name, message.data);
    } else if (message.__uihtml === 'init') {
      revision = message.revision;
      applying = true;
      try {
        currentData = message.value;
      } finally {
        applying = false;
      }
      ready = true;
      if (typeof window.setup === 'function') {
        clearStatus();
        window.setup(htmlComponent);
      } else {
        status('this page defines no setup(htmlComponent) function');
      }
    }
  });

  post({ __uihtml: 'ready' });
  // If the engine never answers, this line is what the user sees instead of an
  // interface that silently does nothing.
  window.setTimeout(function () {
    if (!ready) {
      status('bridge loaded, waiting for the engine');
    }
  }, 3000);
})();
