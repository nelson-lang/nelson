%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% a callback given at creation runs before the constructor returns
f = uifigure('Position', [100 100 480 360], 'AutoResizeChildren', 'off', ...
  'UserData', struct('figure', 0, 'panel', 0), 'SizeChangedFcn', @countFigureResize);
asserts.isequal(f.UserData.figure, 1);
p = uipanel(f, 'Position', [20 20 160 100], 'AutoResizeChildren', 'off', ...
  'SizeChangedFcn', @countPanelResize);
asserts.isequal(f.UserData.panel, 1);
drawnow();
drawnow();
asserts.isequal(f.UserData, struct('figure', 1, 'panel', 1));
%=============================================================================
% a panel resize runs the callback before the assignment returns
p.Position = [20 20 200 120];
asserts.isequal(f.UserData.panel, 2);
% a pure move of a uifigure container is reported too
p.Position = [30 30 200 120];
asserts.isequal(f.UserData.panel, 3);
% the same Position is not a change
p.Position = [30 30 200 120];
drawnow();
asserts.isequal(f.UserData.panel, 3);
asserts.isequal(f.UserData.figure, 1);
%=============================================================================
% an invisible container stays silent, a resize made meanwhile is reported once it is shown
p.Visible = 'off';
p.Position = [30 30 220 140];
p.Position = [30 30 230 150];
drawnow();
asserts.isequal(f.UserData.panel, 3);
p.Visible = 'on';
asserts.isequal(f.UserData.panel, 4);
% hiding and showing without a resize is not a change
p.Visible = 'off';
p.Visible = 'on';
drawnow();
asserts.isequal(f.UserData.panel, 4);
%=============================================================================
% AutoResizeChildren 'on' disables the callback and warns
lastwarn('');
p.AutoResizeChildren = 'on';
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:graphics:sizeChangedFcnDisabled');
p.Position = [30 30 240 150];
drawnow();
asserts.isequal(f.UserData.panel, 4);
lastwarn('');
p.SizeChangedFcn = '';
[msg, id] = lastwarn();
asserts.isequal(id, '');
close(f);
%=============================================================================
% a callback assigned after the creation does not run on its own
f = uifigure('Position', [100 100 480 360], 'AutoResizeChildren', 'off', ...
  'UserData', struct('figure', 0, 'panel', 0));
p = uipanel(f, 'Position', [20 20 160 100], 'AutoResizeChildren', 'off');
f.SizeChangedFcn = @countFigureResize;
p.SizeChangedFcn = @countPanelResize;
drawnow();
drawnow();
asserts.isequal(f.UserData, struct('figure', 0, 'panel', 0));
close(f);
%=============================================================================
% a container created invisible reports its creation when it is shown, in a figure and in a uifigure
f = uifigure('Position', [100 100 480 360], 'AutoResizeChildren', 'off', ...
  'UserData', struct('figure', 0, 'panel', 0));
p = uipanel(f, 'Visible', 'off', 'Position', [20 20 160 100], 'AutoResizeChildren', 'off', ...
  'SizeChangedFcn', @countPanelResize);
drawnow();
asserts.isequal(f.UserData.panel, 0);
p.Visible = 'on';
asserts.isequal(f.UserData.panel, 1);
close(f);
f = figure('Position', [100 100 480 360], 'UserData', struct('figure', 0, 'panel', 0));
p = uipanel(f, 'Visible', 'off', 'Units', 'pixels', 'Position', [20 20 160 100]);
% the callback may be assigned while the container is still hidden
p.SizeChangedFcn = @countPanelResize;
drawnow();
asserts.isequal(f.UserData.panel, 0);
p.Visible = 'on';
asserts.isequal(f.UserData.panel, 1);
close(f);
%=============================================================================
% an invisible container in an invisible figure waits for its own Visible
f = figure('Visible', 'off', 'Position', [100 100 480 360], ...
  'UserData', struct('figure', 0, 'panel', 0));
p = uipanel(f, 'Visible', 'off', 'Units', 'pixels', 'Position', [20 20 160 100], ...
  'SizeChangedFcn', @countPanelResize);
f.Visible = 'on';
drawnow();
asserts.isequal(f.UserData.panel, 0);
p.Visible = 'on';
asserts.isequal(f.UserData.panel, 1);
close(f);
%=============================================================================
% a visible container held by an invisible one waits until that one is shown
f = figure('Position', [100 100 480 360], 'UserData', '');
parent = uipanel(f, 'Units', 'normalized', 'Position', [0 0 0.8 0.8], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'P'));
nested = uipanel(parent, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'n'));
drawnow();
f.UserData = '';
parent.Visible = 'off';
parent.Position = [0 0 0.5 0.5];
f.Position = [100 100 600 450];
nested.Position = [0 0 0.25 0.5];
drawnow();
asserts.isequal(f.UserData, '');
% one event per container, nested containers first
parent.Visible = 'on';
asserts.isequal(f.UserData, 'nP');
% hiding and showing without a resize is not a change
f.UserData = '';
parent.Visible = 'off';
parent.Visible = 'on';
drawnow();
asserts.isequal(f.UserData, '');
% a nested container hidden itself still waits for its own Visible
parent.Visible = 'off';
parent.Position = [0 0 0.6 0.6];
nested.Visible = 'off';
parent.Visible = 'on';
asserts.isequal(f.UserData, 'P');
nested.Visible = 'on';
asserts.isequal(f.UserData, 'Pn');
% a container created in an invisible one reports its creation once that one is shown
f.UserData = '';
parent.Visible = 'off';
created = uipanel(parent, 'Units', 'normalized', 'Position', [0.5 0.5 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'c'));
drawnow();
asserts.isequal(f.UserData, '');
parent.Visible = 'on';
asserts.isequal(f.UserData, 'c');
% an invisible tab group holds the containers of its tabs
group = uitabgroup(f, 'Units', 'normalized', 'Position', [0.6 0 0.4 0.5]);
tab = uitab(group);
inTab = uipanel(tab, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 't'));
drawnow();
f.UserData = '';
group.Visible = 'off';
group.Position = [0.6 0 0.3 0.4];
drawnow();
asserts.isequal(f.UserData, '');
group.Visible = 'on';
asserts.isequal(f.UserData, 't');
close(f);
%=============================================================================
% a resized tab group runs within the statement; its tabs, selected or not, and what they hold
% run once a little later, while events are processed, nested containers first
f = figure('Position', [100 100 480 360], 'UserData', '');
group = uitabgroup(f, 'Units', 'normalized', 'Position', [0 0 0.8 0.8], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'T'));
first = uitab(group, 'SizeChangedFcn', @(s, e) logResize(s, 'a'));
second = uitab(group, 'SizeChangedFcn', @(s, e) logResize(s, 'b'));
inFirst = uipanel(first, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'p'));
nested = uipanel(inFirst, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'n'));
inSecond = uipanel(second, 'Units', 'normalized', 'Position', [0 0 0.5 0.5], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'q'));
drawnow();
f.UserData = '';
group.Position = [0 0 0.5 0.5];
group.Position = [0 0 0.6 0.6];
asserts.isequal(f.UserData, 'TT');
% the rest waits until events have been processed for 20 ms: not at the first drawnow
drawnow();
asserts.isequal(f.UserData, 'TT');
f.UserData = '';
pause(0.1);
asserts.isequal(f.UserData, 'npaqb');
% same when the figure resizes the group
f.UserData = '';
f.Position = [100 100 600 450];
asserts.isequal(f.UserData, 'T');
f.UserData = '';
pause(0.1);
asserts.isequal(f.UserData, 'npaqb');
% two resizes separated by a callback processing are raised separately
f.UserData = '';
group.Position = [0 0 0.5 0.5];
drawnow();
group.Position = [0 0 0.6 0.6];
asserts.isequal(f.UserData, 'TT');
f.UserData = '';
pause(0.15);
asserts.isequal(f.UserData, 'npaqbnpaqb');
% a container resized on its own inside a tab, selected or not, still runs at once
f.UserData = '';
inSecond.Position = [0 0 0.25 0.5];
asserts.isequal(f.UserData, 'q');
inFirst.Position = [0 0 0.25 0.5];
asserts.isequal(f.UserData, 'qnp');
f.UserData = '';
drawnow();
asserts.isequal(f.UserData, '');
% a hidden group resized then shown runs everything within the statement:
% children before their parent, the newest child first
group.Visible = 'off';
group.Position = [0 0 0.4 0.4];
drawnow();
asserts.isequal(f.UserData, '');
group.Visible = 'on';
asserts.isequal(f.UserData, 'qbnpaT');
f.UserData = '';
drawnow();
asserts.isequal(f.UserData, '');
close(f);
% in a uifigure the tabs follow the group the same way
f = uifigure('Position', [100 100 480 360], 'AutoResizeChildren', 'off', 'UserData', '');
group = uitabgroup(f, 'Position', [10 10 200 100], 'AutoResizeChildren', 'off', ...
  'SizeChangedFcn', @(s, e) logResize(s, 'T'));
first = uitab(group, 'AutoResizeChildren', 'off', 'SizeChangedFcn', @(s, e) logResize(s, 'a'));
second = uitab(group, 'AutoResizeChildren', 'off', 'SizeChangedFcn', @(s, e) logResize(s, 'b'));
drawnow();
f.UserData = '';
group.Position = [10 10 250 120];
asserts.isequal(f.UserData, 'T');
% a uifigure tab keeps reporting its former size until its own event;
% that size is its page: the group less a 1 pixel border and a 23 pixel bar
asserts.isequal(first.Position, [1 1 198 75]);
asserts.isequal(first.InnerPosition, [1 1 198 75]);
asserts.isequal(first.OuterPosition, [1 1 198 75]);
asserts.isequal(getpixelposition(first), [1 1 198 75]);
f.UserData = '';
pause(0.1);
asserts.isequal(f.UserData, 'ab');
asserts.isequal(first.Position(3:4), [248 95]);
asserts.isequal(second.Position(3:4), [248 95]);
% each event reports the size of its own resize
first.SizeChangedFcn = @(s, e) logResize(s, sprintf('%d,', s.Position(3)));
second.SizeChangedFcn = '';
f.UserData = '';
group.Position = [10 10 260 120];
drawnow();
group.Position = [10 10 270 120];
asserts.isequal(first.Position(3), 248);
pause(0.15);
asserts.isequal(f.UserData, 'TT258,268,');
asserts.isequal(first.Position(3), 268);
first.SizeChangedFcn = @(s, e) logResize(s, 'a');
second.SizeChangedFcn = @(s, e) logResize(s, 'b');
% a uifigure panel or button group reports a pure move, a tab group does not
f.UserData = '';
group.Position = [30 40 270 120];
drawnow();
asserts.isequal(f.UserData, '');
buttons = uibuttongroup(f, 'Position', [300 100 80 80], 'AutoResizeChildren', 'off', ...
  'SizeChangedFcn', @(s, e) logResize(s, 'B'));
f.UserData = '';
buttons.Position = [310 100 80 80];
asserts.isequal(f.UserData, 'B');
close(f);
% in a figure a moved tab group is silent as well
f = figure('Position', [100 100 480 360], 'UserData', '');
group = uitabgroup(f, 'Units', 'pixels', 'Position', [10 10 200 100], ...
  'SizeChangedFcn', @(s, e) logResize(s, 'T'));
first = uitab(group, 'SizeChangedFcn', @(s, e) logResize(s, 'a'));
drawnow();
f.UserData = '';
group.Position = [30 40 200 100];
drawnow();
asserts.isequal(f.UserData, '');
close(f);
%=============================================================================
% uifigure containers auto resize their children by default, figure ones do not
f = uifigure('Visible', 'off');
p = uipanel(f);
tg = uitabgroup(f);
t = uitab(tg);
asserts.isequal(char(p.AutoResizeChildren), 'on');
asserts.isequal(char(t.AutoResizeChildren), 'on');
close(f);
f = figure('Visible', 'off');
p = uipanel(f);
asserts.isequal(char(p.AutoResizeChildren), 'off');
close(f);
%=============================================================================
% nothing runs while the figure is invisible, everything once it is shown
f = figure('Visible', 'off', 'Position', [100 100 480 360], ...
  'UserData', struct('figure', 0, 'panel', 0), 'SizeChangedFcn', @countFigureResize);
p = uipanel(f, 'Units', 'pixels', 'Position', [20 20 160 100], ...
  'SizeChangedFcn', @countPanelResize);
drawnow();
p.Position = [20 20 200 120];
drawnow();
asserts.isequal(f.UserData, struct('figure', 0, 'panel', 0));
f.Visible = 'on';
asserts.isequal(f.UserData, struct('figure', 1, 'panel', 1));
% a figure panel is silent on a pure move
p.Position = [30 30 200 120];
drawnow();
asserts.isequal(f.UserData.panel, 1);
close(f);
%=============================================================================
function countFigureResize(source, event)
  data = source.UserData;
  data.figure = data.figure + 1;
  source.UserData = data;
end
%=============================================================================
function logResize(source, tag)
  f = ancestor(source, 'figure');
  f.UserData = [f.UserData, tag];
end
%=============================================================================
function countPanelResize(source, event)
  f = ancestor(source, 'figure');
  data = f.UserData;
  data.panel = data.panel + 1;
  f.UserData = data;
end
%=============================================================================
