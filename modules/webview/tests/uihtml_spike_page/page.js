// Loaded through a relative src. Defines the global setup(htmlComponent) the
// uihtml contract expects; the spike test drives it from Nelson.

function setup(htmlComponent) {
  // Report once, so the test knows setup() ran, what Data it started with, and
  // whether the relative stylesheet was applied.
  var probe = document.getElementById('probe');
  var color = window.getComputedStyle(probe).color;
  htmlComponent.sendEventToNelson('pageReady', {
    color: color,
    initialData: htmlComponent.Data
  });

  // Nelson wrote Data: the page is notified and echoes what it saw. If the
  // engine also treated this as a page write, the counters would diverge.
  htmlComponent.addEventListener('DataChanged', function () {
    htmlComponent.sendEventToNelson('sawData', htmlComponent.Data);
  });

  // A page-originated write, on request. Used to check that a write coming
  // from here notifies Nelson and does not bounce back as a host push.
  htmlComponent.addEventListener('bounce', function (event) {
    htmlComponent.Data = event.Data;
  });

  // Plain event round trip with a structured payload.
  htmlComponent.addEventListener('ping', function (event) {
    htmlComponent.sendEventToNelson('pong', event.Data);
  });
}
