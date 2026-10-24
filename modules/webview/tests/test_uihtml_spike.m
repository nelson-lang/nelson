%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--WITH DISPLAY-->
%=============================================================================
% Spike for the future uihtml component. Proves the three things that were
% actually uncertain: the Data round trip closes without an echo loop, a user
% HTML file keeps its relative assets, and setup(htmlComponent) is replayed
% when the source changes. Everything else about uihtml is out of scope here.
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'GUI'), _('uihtml spike requires GUI mode.'));
skip_testsuite(isempty(which('__uihtml_spike__')), _('webview module not loaded.'));
%=============================================================================
% The spike never falls back to a web browser, so a machine without a native
% WebView backend simply cannot run this.
% Creating a WebView controller can fail transiently (the vendored backend
% documents a retryable 0x8007139F), so a single failed probe must not skip the
% whole suite and hide a real regression.
canOpen = false;
for attempt = 1:3
  try
    probeId = __uihtml_spike__('open', '<html><body>probe</body></html>');
    __uihtml_spike__('close', probeId);
    canOpen = true;
    break;
  catch
    sleep(1);
  end
end
skip_testsuite(~canOpen, _('no native WebView backend available.'));
%=============================================================================
cleaner = onCleanup(@() __uihtml_spike__('closeall'));
TIMEOUT = 30;
%=============================================================================
% S1 + S6: a real file, with a relative stylesheet and a relative script.
pageFile = [modulepath('webview', 'tests'), '/uihtml_spike_page/page.html'];
asserts.istrue(isfile(pageFile));
id = __uihtml_spike__('open', pageFile);

started = tic();
stats = __uihtml_spike__('stats', id);
while (stats.setupCount < 1) && (toc(started) < TIMEOUT)
  sleep(0.05);
  stats = __uihtml_spike__('stats', id);
end
asserts.isequal(stats.setupCount, 1);
asserts.istrue(stats.bridgeAvailable);

started = tic();
[names, payloads] = __uihtml_spike__('events', id);
while isempty(names) && (toc(started) < TIMEOUT)
  sleep(0.05);
  [names, payloads] = __uihtml_spike__('events', id);
end
% page.js only runs if the relative <script src> was served, and it only
% reports rgb(1, 2, 3) if the relative <link rel=stylesheet> was too.
asserts.isequal(names{1}, 'pageReady');
ready = jsondecode(payloads{1});
asserts.isequal(ready.color, 'rgb(1, 2, 3)');
%=============================================================================
% S2 + S4: host writes never come back as page writes.
% The counters are cumulative and pageReady already moved eventsFromPage, so
% everything below is measured as a delta from here.
base = __uihtml_spike__('stats', id);
ROUNDS = 25;
for k = 1:ROUNDS
  __uihtml_spike__('setdata', id, sprintf('{"k":%d}', k));
end

started = tic();
stats = __uihtml_spike__('stats', id);
while (stats.eventsFromPage - base.eventsFromPage < ROUNDS) && (toc(started) < TIMEOUT)
  sleep(0.05);
  stats = __uihtml_spike__('stats', id);
end
% Every host write reached the page exactly once ...
asserts.isequal(stats.dataToPage - base.dataToPage, ROUNDS);
asserts.isequal(stats.eventsFromPage - base.eventsFromPage, ROUNDS);
% ... and none of them was reported back as a page-originated write.
asserts.isequal(stats.dataFromPage, 0);
asserts.isequal(__uihtml_spike__('getdata', id), sprintf('{"k":%d}', ROUNDS));

[names, payloads] = __uihtml_spike__('events', id);
asserts.isequal(numel(names), ROUNDS);
% Page events must arrive IN ORDER. WebUI runs each bound-function event on its
% own thread and those race, so the bootstrap keeps a single call in flight and
% stamps a sequence the host checks.
seenK = zeros(1, ROUNDS);
for k = 1:ROUNDS
  asserts.isequal(names{k}, 'sawData');
  entry = jsondecode(payloads{k});
  seenK(k) = entry.k;
end
asserts.isequal(seenK, 1:ROUNDS);
asserts.isequal(stats.outOfOrder, 0);
%=============================================================================
% S3: page writes notify Nelson and are not pushed back down.
before = __uihtml_spike__('stats', id);
for k = 1:ROUNDS
  __uihtml_spike__('sendevent', id, 'bounce', sprintf('{"b":%d}', k));
end

started = tic();
stats = __uihtml_spike__('stats', id);
while (stats.dataFromPage - before.dataFromPage < ROUNDS) && (toc(started) < TIMEOUT)
  sleep(0.05);
  stats = __uihtml_spike__('stats', id);
end
asserts.isequal(stats.dataFromPage - before.dataFromPage, ROUNDS);
% No host push answered them, so the page never re-notified itself: had it,
% dataToPage would have grown and sawData events would have piled up.
asserts.isequal(stats.dataToPage, before.dataToPage);
asserts.isequal(stats.eventsFromPage, before.eventsFromPage);
% Page writes are ordered too, so the host ends up holding the last one sent.
bounced = jsondecode(__uihtml_spike__('getdata', id));
asserts.isequal(bounced.b, ROUNDS);
asserts.isequal(stats.outOfOrder, 0);

before = __uihtml_spike__('stats', id);
__uihtml_spike__('sendevent', id, 'bounce', '{"b":999}');
started = tic();
stats = __uihtml_spike__('stats', id);
while (stats.dataFromPage == before.dataFromPage) && (toc(started) < TIMEOUT)
  sleep(0.05);
  stats = __uihtml_spike__('stats', id);
end
settled = jsondecode(__uihtml_spike__('getdata', id));
asserts.isequal(settled.b, 999);
%=============================================================================
% S5: events both ways, structured payload preserved.
sent = struct('label', 'ping', 'values', [1 2 3]);
__uihtml_spike__('sendevent', id, 'ping', jsonencode(sent));

started = tic();
[names, payloads] = __uihtml_spike__('events', id);
while isempty(names) && (toc(started) < TIMEOUT)
  sleep(0.05);
  [names, payloads] = __uihtml_spike__('events', id);
end
asserts.isequal(names{1}, 'pong');
received = jsondecode(payloads{1});
asserts.isequal(received.label, 'ping');
asserts.isequal(received.values(:)', [1 2 3]);
%=============================================================================
% S7: changing the source replays setup() and hands it the current Data.
before = __uihtml_spike__('stats', id);
secondPage = ['<html><body><script>function setup(h) {', ...
  ' h.sendEventToNelson("secondPage", h.Data); }</script></body></html>'];
__uihtml_spike__('source', id, secondPage);

started = tic();
stats = __uihtml_spike__('stats', id);
while (stats.setupCount <= before.setupCount) && (toc(started) < TIMEOUT)
  sleep(0.05);
  stats = __uihtml_spike__('stats', id);
end
asserts.isequal(stats.setupCount, before.setupCount + 1);

started = tic();
[names, payloads] = __uihtml_spike__('events', id);
while isempty(names) && (toc(started) < TIMEOUT)
  sleep(0.05);
  [names, payloads] = __uihtml_spike__('events', id);
end
asserts.isequal(names{1}, 'secondPage');
carried = jsondecode(payloads{1});
asserts.isequal(carried.b, 999);
%=============================================================================
% S8: close.
asserts.istrue(__uihtml_spike__('isopen', id));
__uihtml_spike__('close', id);
asserts.isfalse(__uihtml_spike__('isopen', id));
%=============================================================================
% S9: several windows at once, closed out of order.
first = __uihtml_spike__('open', '<html><body>first</body></html>');
second = __uihtml_spike__('open', '<html><body>second</body></html>');
third = __uihtml_spike__('open', '<html><body>third</body></html>');
asserts.isequal(numel(unique([first, second, third])), 3);
% A window only counts as shown once its page has connected back, so opening
% and asking straight away is always false.
started = tic();
while ~(__uihtml_spike__('isopen', first) && __uihtml_spike__('isopen', second) ...
    && __uihtml_spike__('isopen', third)) && (toc(started) < TIMEOUT)
  sleep(0.05);
end
asserts.istrue(__uihtml_spike__('isopen', second));
__uihtml_spike__('close', second);
asserts.isfalse(__uihtml_spike__('isopen', second));
asserts.istrue(__uihtml_spike__('isopen', first));
__uihtml_spike__('close', third);
__uihtml_spike__('close', first);
asserts.isfalse(__uihtml_spike__('isopen', first));
%=============================================================================
% Bad input is reported, not silently accepted.
asserts.checkerror('__uihtml_spike__(''open'', ''no_such_file_here.html'');', ...
  'HTMLSource must be HTML text, an existing file, or an http(s) URL.');
asserts.checkerror('__uihtml_spike__(''nope'', 1);', 'Unknown uihtml spike command.');
%=============================================================================
