%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Run by test_web_figure_size_changed_fcn in a child that renders figures as web windows.
try
  checks = {@checkClassicFigure, @checkInvisibleParent, @checkInvisibleFigure, ...
    @checkUiFigure, @checkTabGroup};
  for k = 1:numel(checks)
    % a child stopped by the timeout leaves the name of the check it was running
    check = checks{k};
    writeResult(['RUNNING: ', func2str(check)]);
    check();
  end
  writeResult('OK');
  exit(0);
catch
  e = lasterror();
  writeResult(['FAILED: ', e.message]);
  exit(1);
end
%=============================================================================
function checkClassicFigure()
  % callbacks given at creation run before the constructor returns
  f = figure('Position', [10 20 400 300], 'UserData', '', 'SizeChangedFcn', @(s, e) record(s, 'F'));
  asserts.isequal(takeLog(f), 'F');
  normalized = uipanel(f, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'N'));
  asserts.isequal(takeLog(f), 'N');
  pixels = uipanel(f, 'Units', 'pixels', 'Position', [210 10 100 100], ...
    'SizeChangedFcn', @(s, e) record(s, 'P'));
  asserts.isequal(takeLog(f), 'P');
  nested = uipanel(normalized, 'Units', 'normalized', 'Position', [0 0 0.5 0.5]);
  nested.SizeChangedFcn = @(s, e) record(s, 'n');
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % a script resize runs the containers whose pixel size changed, then the figure
  f.Position = [10 20 600 300];
  asserts.isequal(takeLog(f), 'nNF');
  % a move or the same Position is not a resize
  f.Position = [30 40 600 300];
  f.Position = [30 40 600 300];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % a size reported by the browser is queued until the next drawnow
  handle = __web_graphics_handle__(f, 'number');
  f.SizeChangedFcn = @(s, e) record(s, sprintf('F%dx%d', e.Width, e.Height));
  asserts.istrue(__web_figure_size__(handle, 500, 250));
  asserts.isequal(takeLog(f), '');
  drainCallbacks();
  asserts.isequal(takeLog(f), 'nNF500x250');
  asserts.isequal(f.Position, [30 40 600 300]);
  asserts.istrue(__web_figure_size__(handle, 500, 250));
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % no event for an empty figure callback, the containers still follow
  f.SizeChangedFcn = '';
  asserts.istrue(__web_figure_size__(handle, 520, 250));
  drainCallbacks();
  asserts.isequal(takeLog(f), 'nN');
  % a container resize is synchronous, nested containers first
  normalized.Position = [0 0 0.25 0.5];
  asserts.isequal(takeLog(f), 'nN');
  pixels.Position = [210 10 120 100];
  asserts.isequal(takeLog(f), 'P');
  % a pure move in a classic figure and an invisible container stay silent
  pixels.Position = [220 10 120 100];
  pixels.Visible = 'off';
  pixels.Position = [220 10 140 100];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  close(f);
end
%=============================================================================
function checkInvisibleParent()
  % a visible container held by an invisible one waits until that one is shown
  f = figure('Position', [10 20 400 300], 'UserData', '');
  parent = uipanel(f, 'Units', 'normalized', 'Position', [0 0 0.8 0.8], ...
    'SizeChangedFcn', @(s, e) record(s, 'P'));
  nested = uipanel(parent, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'n'));
  takeLog(f);
  parent.Visible = 'off';
  parent.Position = [0 0 0.5 0.5];
  f.Position = [10 20 600 300];
  nested.Position = [0 0 0.25 0.5];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % one event per container, nested containers first
  parent.Visible = 'on';
  asserts.isequal(takeLog(f), 'nP');
  % hiding and showing without a resize is not a change
  parent.Visible = 'off';
  parent.Visible = 'on';
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % a nested container hidden itself still waits for its own Visible
  parent.Visible = 'off';
  parent.Position = [0 0 0.6 0.6];
  nested.Visible = 'off';
  parent.Visible = 'on';
  asserts.isequal(takeLog(f), 'P');
  nested.Visible = 'on';
  asserts.isequal(takeLog(f), 'n');
  % a container created in an invisible one reports its creation once that one is shown
  parent.Visible = 'off';
  created = uipanel(parent, 'Units', 'normalized', 'Position', [0.5 0.5 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'c'));
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  parent.Visible = 'on';
  asserts.isequal(takeLog(f), 'c');
  close(f);
end
%=============================================================================
function checkInvisibleFigure()
  % everything is held until the figure is shown
  f = figure('Visible', 'off', 'Position', [10 20 400 300], 'UserData', '', ...
    'SizeChangedFcn', @(s, e) record(s, 'F'));
  p = uipanel(f, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'N'));
  f.Position = [10 20 500 300];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  f.Visible = 'on';
  asserts.isequal(takeLog(f), 'NF');
  close(f);
end
%=============================================================================
function checkUiFigure()
  f = uifigure('Position', [10 20 400 300], 'UserData', '');
  lastwarn('');
  f.SizeChangedFcn = @(s, e) record(s, 'U');
  [msg, id] = lastwarn();
  asserts.isequal(id, 'Nelson:graphics:sizeChangedFcnDisabled');
  handle = __web_graphics_handle__(f, 'number');
  % AutoResizeChildren 'on' disables the callback on every path
  f.Position = [10 20 450 300];
  asserts.istrue(__web_figure_size__(handle, 460, 300));
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  f.AutoResizeChildren = 'off';
  f.Position = [10 20 500 300];
  asserts.isequal(takeLog(f), 'U');
  asserts.istrue(__web_figure_size__(handle, 520, 300));
  asserts.isequal(takeLog(f), '');
  drainCallbacks();
  asserts.isequal(takeLog(f), 'U');
  % a panel held by a grid follows its cell
  grid = uigridlayout(f, [1 2]);
  cell = uipanel(grid, 'AutoResizeChildren', 'off', 'SizeChangedFcn', @(s, e) record(s, 'G'));
  drainCallbacks();
  takeLog(f);
  f.Position = [10 20 700 300];
  asserts.isequal(takeLog(f), 'GU');
  % a tab group reports its resize at once, its tabs a little later
  group = uitabgroup(f, 'Position', [10 10 200 100], 'AutoResizeChildren', 'off', ...
    'SizeChangedFcn', @(s, e) record(s, 'T'));
  tab = uitab(group, 'AutoResizeChildren', 'off', 'SizeChangedFcn', @(s, e) record(s, 't'));
  drainCallbacks();
  takeLog(f);
  group.Position = [10 10 250 100];
  asserts.isequal(takeLog(f), 'T');
  % a uifigure tab keeps reporting its former size until its own event
  asserts.isequal(tab.Position, [1 1 198 75]);
  asserts.isequal(getpixelposition(tab), [1 1 198 75]);
  waitDeferred();
  asserts.isequal(takeLog(f), 't');
  asserts.isequal(tab.Position, [1 1 248 75]);
  % a uifigure panel or button group reports a pure move, a tab group does not
  group.Position = [20 30 250 100];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  moved = uipanel(f, 'Position', [300 10 80 80], 'AutoResizeChildren', 'off', ...
    'SizeChangedFcn', @(s, e) record(s, 'M'));
  buttons = uibuttongroup(f, 'Position', [300 100 80 80], 'AutoResizeChildren', 'off', ...
    'SizeChangedFcn', @(s, e) record(s, 'B'));
  takeLog(f);
  moved.Position = [310 10 80 80];
  asserts.isequal(takeLog(f), 'M');
  buttons.Position = [310 100 80 80];
  asserts.isequal(takeLog(f), 'B');
  close(f);
end
%=============================================================================
function checkTabGroup()
  f = figure('Position', [10 20 400 300], 'UserData', '');
  group = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 0.8 0.8], ...
    'SizeChangedFcn', @(s, e) record(s, 'T'));
  first = uitab(group, 'SizeChangedFcn', @(s, e) record(s, 'a'));
  second = uitab(group, 'SizeChangedFcn', @(s, e) record(s, 'b'));
  inFirst = uipanel(first, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'p'));
  nested = uipanel(inFirst, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'n'));
  inSecond = uipanel(second, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
    'SizeChangedFcn', @(s, e) record(s, 'q'));
  drainCallbacks();
  takeLog(f);
  % only the group runs within the statement; every tab, selected or not, and what it holds
  % run once a little later, nested containers first, and never at the first drawnow
  group.Position = [0 0 0.5 0.5];
  group.Position = [0 0 0.6 0.6];
  drawnow();
  asserts.isequal(takeLog(f), 'TT');
  waitDeferred();
  asserts.isequal(takeLog(f), 'npaqb');
  % same when the figure resizes the group
  f.Position = [10 20 600 300];
  asserts.isequal(takeLog(f), 'T');
  waitDeferred();
  asserts.isequal(takeLog(f), 'npaqb');
  % two resizes separated by a callback processing are raised separately
  group.Position = [0 0 0.5 0.5];
  drawnow();
  group.Position = [0 0 0.6 0.6];
  asserts.isequal(takeLog(f), 'TT');
  waitDeferred();
  asserts.isequal(takeLog(f), 'npaqbnpaqb');
  % a container resized on its own inside a tab, selected or not, still runs at once
  inSecond.Position = [0 0 0.25 0.5];
  asserts.isequal(takeLog(f), 'q');
  inFirst.Position = [0 0 0.25 0.5];
  asserts.isequal(takeLog(f), 'np');
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  % a hidden group resized then shown runs everything within the statement:
  % children before their parent, the newest child first
  group.Visible = 'off';
  group.Position = [0 0 0.4 0.4];
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  group.Visible = 'on';
  asserts.isequal(takeLog(f), 'qbnpaT');
  drainCallbacks();
  asserts.isequal(takeLog(f), '');
  close(f);
end
%=============================================================================
% the console output of an exiting child can be lost: the verdict goes to a file
function writeResult(text)
  disp(text);
  name = getenv('NELSON_WEB_SIZE_CHANGED_FCN_RESULT');
  if ~isempty(name)
    fid = fopen(name, 'wt');
    fprintf(fid, '%s', text);
    fclose(fid);
  end
end
%=============================================================================
function record(source, tag)
  f = ancestor(source, 'figure');
  f.UserData = [f.UserData, tag];
end
%=============================================================================
function log = takeLog(f)
  log = f.UserData;
  f.UserData = '';
end
%=============================================================================
function drainCallbacks()
  % the queue may run a single callback per drawnow
  for k = 1:6
    drawnow();
  end
end
%=============================================================================
function waitDeferred()
  % what a tab group resizes runs once events have been processed for 20 ms
  pause(0.1);
  drainCallbacks();
end
%=============================================================================
