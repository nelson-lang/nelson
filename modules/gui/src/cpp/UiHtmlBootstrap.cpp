//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UiHtmlBootstrap.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
const char* UIHTML_BOOTSTRAP_PATH = "/__nelson_uihtml_bootstrap__.js";
//=============================================================================
static size_t
findCaseInsensitive(const std::string& haystack, const std::string& needle, size_t from = 0);
//=============================================================================
static size_t
endOfBodyOpeningTag(const std::string& document);
//=============================================================================
// The bridge injected into every uihtml page.
//
// Two guards keep the Data round trip from echoing forever:
//   - `applying` is raised while a host-originated value is being installed, so
//     the setter does not report that value straight back to the host;
//   - `revision` is stamped by the host and only ever grows, so a stale or
//     duplicated push is ignored.
// They rely on the contract being asymmetric: a page write notifies the host
// only, a host write notifies the page only. Symmetric notification cannot be
// made loop-free by any amount of guarding.
//
// `boot()` PULLS the initial state instead of waiting for a push: `webui_run`
// is fire-and-forget and is dropped while the socket is still connecting, so a
// page that waited for one would start empty roughly one load in three.
static const std::string bootstrapScript = R"JSBOOT((function () {
  'use strict';

  var listeners = {};
  var currentData = null;
  var revision = 0;
  var applying = false;
  var ready = false;

  // Page -> engine calls are SERIALIZED. The page contract expects page events in
  // order; WebUI does not, because it runs each bound-function event on its own
  // thread and those race for the lock. Keeping a single call in flight restores
  // ordering by construction, and the sequence stamp lets the host verify it.
  var sendChain = Promise.resolve();
  var sequence = 0;

  function call(kind, payload) {
    sequence = sequence + 1;
    var envelope = JSON.stringify({ kind: kind, sequence: sequence, payload: payload });
    var result = sendChain.then(function () {
      return window.webui.call('uihtmlRpc', envelope);
    }, function () {
      return window.webui.call('uihtmlRpc', envelope);
    });
    // Keep the chain alive even when one call fails.
    sendChain = result.then(function () { }, function () { });
    return result;
  }

  function dispatch(name, detail) {
    var handlers = listeners[name];
    if (!handlers) {
      return;
    }
    for (var i = 0; i < handlers.length; i++) {
      try {
        // Same event shape: the page listener gets Source and Data.
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
      if (applying) {
        return;
      }
      call('data', { value: value });
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
      call('event', { name: name, data: data });
    }
  };

  window.__nelsonUiHtml = {
    applyData: function (json, newRevision) {
      if (newRevision <= revision) {
        return;
      }
      revision = newRevision;
      applying = true;
      try {
        currentData = JSON.parse(json);
        if (ready) {
          dispatch('DataChanged', currentData);
        }
      } finally {
        applying = false;
      }
    },
    applyEvent: function (name, json) {
      dispatch(name, JSON.parse(json));
    },
    component: htmlComponent
  };

  // The bridge object exists as soon as /webui.js runs, but its WebSocket
  // connects asynchronously and a call placed before that is lost. Poll the
  // same way the examples gallery does rather than calling on 'load'.
  function transportReady() {
    return typeof window.webui !== 'undefined'
      && typeof window.webui.call === 'function'
      && (typeof window.webui.isConnected !== 'function' || window.webui.isConnected());
  }

  function boot(attempt) {
    if (!transportReady()) {
      if (attempt > 200) {
        console.error('uihtml: the WebUI bridge never connected');
        return;
      }
      window.setTimeout(function () {
        boot(attempt + 1);
      }, 50);
      return;
    }
    call('ready', {}).then(function (text) {
      var state = JSON.parse(text);
      revision = state.revision;
      applying = true;
      try {
        currentData = state.data;
      } finally {
        applying = false;
      }
      ready = true;
      if (typeof window.setup === 'function') {
        window.setup(htmlComponent);
      } else {
        console.warn('uihtml: this page defines no setup(htmlComponent) function');
      }
    });
  }

  if (document.readyState === 'complete') {
    boot(0);
  } else {
    window.addEventListener('load', function () {
      boot(0);
    });
  }
})();
)JSBOOT";
//=============================================================================
const std::string&
UiHtmlBootstrapScript()
{
    return bootstrapScript;
}
//=============================================================================
std::string
UiHtmlInjectBootstrap(const std::string& document)
{
    std::string tags = "<script src=\"/webui.js\"></script>\n<script src=\"";
    tags += UIHTML_BOOTSTRAP_PATH;
    tags += "\"></script>\n";

    const size_t headEnd = findCaseInsensitive(document, "</head>");
    if (headEnd != std::string::npos) {
        return document.substr(0, headEnd) + tags + document.substr(headEnd);
    }
    const size_t bodyStart = endOfBodyOpeningTag(document);
    if (bodyStart != std::string::npos) {
        return document.substr(0, bodyStart) + "\n" + tags + document.substr(bodyStart);
    }
    return tags + document;
}
//=============================================================================
static size_t
findCaseInsensitive(const std::string& haystack, const std::string& needle, size_t from)
{
    if (needle.empty() || haystack.size() < needle.size()) {
        return std::string::npos;
    }
    const auto equalsIgnoringCase = [](char left, char right) {
        return std::tolower(static_cast<unsigned char>(left))
            == std::tolower(static_cast<unsigned char>(right));
    };
    const auto found = std::search(haystack.begin() + static_cast<long>(from), haystack.end(),
        needle.begin(), needle.end(), equalsIgnoringCase);
    if (found == haystack.end()) {
        return std::string::npos;
    }
    return static_cast<size_t>(std::distance(haystack.begin(), found));
}
//=============================================================================
static size_t
endOfBodyOpeningTag(const std::string& document)
{
    const size_t bodyTag = findCaseInsensitive(document, "<body");
    if (bodyTag == std::string::npos) {
        return std::string::npos;
    }
    const size_t close = document.find('>', bodyTag);
    if (close == std::string::npos) {
        return std::string::npos;
    }
    return close + 1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
