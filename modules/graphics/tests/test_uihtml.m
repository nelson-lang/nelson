%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The uihtml contract. Every assertion below mirrors a behaviour that was
% measured before the component was written, not a behaviour invented for it.
%=============================================================================
skip_testsuite(~strcmp(getnelsonmode(), 'WEB_GUI'), _('uihtml requires the WebView desktop.'));
%=============================================================================
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
TIMEOUT = 30;
f = uifigure('Visible', 'off');
cleaner = onCleanup(@() close(f));
%=============================================================================
% Surface and defaults.
h = uihtml(f);
asserts.isequal(get(h, 'Type'), 'uihtml');
asserts.isequal(class(h), 'nelson.ui.control.HTML');
asserts.isequal(get(h, 'HTMLSource'), '');
asserts.istrue(isempty(get(h, 'Data')));
asserts.isequal(get(h, 'Position'), [100 100 100 100]);
%=============================================================================
% HTMLSource keeps what it is given, markup or file name alike.
markup = '<html><body><p>hello</p></body></html>';
set(h, 'HTMLSource', markup);
asserts.isequal(get(h, 'HTMLSource'), markup);
set(h, 'HTMLSource', 'some_page.html');
asserts.isequal(get(h, 'HTMLSource'), 'some_page.html');
%=============================================================================
% Data is carried as JSON, so it comes back as jsonencode/jsondecode leave it.
% These are the documented losses, locked here so they cannot drift silently.
set(h, 'Data', struct('a', 1, 'b', 'text'));
back = get(h, 'Data');
asserts.isequal(back.a, 1);
asserts.isequal(back.b, 'text');
%=============================================================================
% A live page: setup() runs, and the engine to page direction reaches it.
page = ['<html><body><script>', ...
  'function setup(h) {', ...
  '  h.addEventListener("DataChanged", function () {', ...
  '    h.sendEventToNelson("sawData", h.Data);', ...
  '  });', ...
  '  h.addEventListener("bounce", function (e) { h.Data = e.Data; });', ...
  '  h.addEventListener("burst", function (e) {', ...
  '    for (var i = 1; i <= e.Data.n; i++) { h.sendEventToNelson("tick", { i: i }); }', ...
  '  });', ...
  '  h.sendEventToNelson("ready", {});', ...
  '}</script></body></html>'];

global UIHTML_EVENTS
global UIHTML_DATA_CHANGES
UIHTML_EVENTS = {};
UIHTML_DATA_CHANGES = {};
p = uihtml(f, 'Position', [10 10 400 300]);
set(p, 'HTMLEventReceivedFcn', @uihtmlRecordEvent);
set(p, 'DataChangedFcn', @uihtmlRecordDataChange);
set(p, 'HTMLSource', page);
set(f, 'Visible', 'on');

started = tic();
while isempty(UIHTML_EVENTS) && (toc(started) < TIMEOUT)
  drawnow();
  sleep(0.05);
end
asserts.isfalse(isempty(UIHTML_EVENTS));
asserts.isequal(UIHTML_EVENTS{1}.name, 'ready');
%=============================================================================
% An engine write notifies the page and does NOT fire DataChangedFcn.
UIHTML_EVENTS = {};
UIHTML_DATA_CHANGES = {};
set(p, 'Data', struct('k', 1));
started = tic();
while isempty(UIHTML_EVENTS) && (toc(started) < TIMEOUT)
  drawnow();
  sleep(0.05);
end
asserts.isequal(UIHTML_EVENTS{1}.name, 'sawData');
asserts.istrue(isempty(UIHTML_DATA_CHANGES));
%=============================================================================
% Writing the same value again notifies nobody.
UIHTML_EVENTS = {};
set(p, 'Data', struct('k', 1));
for k = 1:10
  drawnow();
  sleep(0.05);
end
asserts.istrue(isempty(UIHTML_EVENTS));
%=============================================================================
% A page write fires DataChangedFcn once, carries PreviousData, and is not
% echoed back down.
UIHTML_EVENTS = {};
UIHTML_DATA_CHANGES = {};
sendEventToHTMLSource(p, 'bounce', struct('k', 2));
started = tic();
while isempty(UIHTML_DATA_CHANGES) && (toc(started) < TIMEOUT)
  drawnow();
  sleep(0.05);
end
asserts.isequal(numel(UIHTML_DATA_CHANGES), 1);
asserts.isequal(UIHTML_DATA_CHANGES{1}.data.k, 2);
asserts.isequal(UIHTML_DATA_CHANGES{1}.previous.k, 1);
asserts.istrue(isempty(UIHTML_EVENTS));
%=============================================================================
% Page events arrive in the order the page sent them.
UIHTML_EVENTS = {};
ROUNDS = 25;
sendEventToHTMLSource(p, 'burst', struct('n', ROUNDS));
started = tic();
while (numel(UIHTML_EVENTS) < ROUNDS) && (toc(started) < TIMEOUT)
  drawnow();
  sleep(0.05);
end
asserts.isequal(numel(UIHTML_EVENTS), ROUNDS);
order = zeros(1, ROUNDS);
for k = 1:ROUNDS
  asserts.isequal(UIHTML_EVENTS{k}.name, 'tick');
  order(k) = UIHTML_EVENTS{k}.data.i;
end
asserts.isequal(order, 1:ROUNDS);
%=============================================================================
% The page survives being hidden: no reload, so no second 'ready'.
UIHTML_EVENTS = {};
set(p, 'Visible', 'off');
drawnow(); sleep(0.5);
set(p, 'Visible', 'on');
for k = 1:10
  drawnow();
  sleep(0.05);
end
for k = 1:numel(UIHTML_EVENTS)
  asserts.isfalse(strcmp(UIHTML_EVENTS{k}.name, 'ready'));
end
%=============================================================================
clear UIHTML_EVENTS UIHTML_DATA_CHANGES;
%=============================================================================
