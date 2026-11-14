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
% drawnow and pause run every callback pending when they start, not one per call
f1 = figure('Visible', 'off', 'Position', [10 20 920 580]);
f2 = figure('Visible', 'off', 'Position', [10 20 920 580]);
f3 = figure('Visible', 'off', 'Position', [10 20 920 580]);
figs = [f1, f2, f3];
handles = zeros(1, 3);
for k = 1:3
  figs(k).UserData = 0;
  figs(k).SizeChangedFcn = @countCall;
  handles(k) = __web_graphics_handle__(figs(k), 'number');
end
drawnow();
%=============================================================================
% three queued callbacks, one drawnow
for k = 1:3
  figs(k).UserData = 0;
  asserts.istrue(__web_figure_size__(handles(k), 600, 500));
end
asserts.isequal([f1.UserData, f2.UserData, f3.UserData], [0 0 0]);
drawnow();
asserts.isequal([f1.UserData, f2.UserData, f3.UserData], [1 1 1]);
drawnow();
asserts.isequal([f1.UserData, f2.UserData, f3.UserData], [1 1 1]);
%=============================================================================
% three queued callbacks, one pause
for k = 1:3
  asserts.istrue(__web_figure_size__(handles(k), 640, 520));
end
asserts.isequal([f1.UserData, f2.UserData, f3.UserData], [1 1 1]);
pause(0.1);
asserts.isequal([f1.UserData, f2.UserData, f3.UserData], [2 2 2]);
%=============================================================================
% a callback that queues itself again does not hang the drain
f1.UserData = 0;
f1.SizeChangedFcn = @(src, evt) requeue(src, handles(1));
asserts.istrue(__web_figure_size__(handles(1), 650, 500));
drawnow();
asserts.istrue(f1.UserData >= 1);
for k = 1:30
  drawnow();
end
asserts.isequal(f1.UserData, 10);
%=============================================================================
% timer callbacks queued while the script is busy all run in one drawnow
global timerDrainCount
timerDrainCount = 0;
for k = 1:4
  t(k) = timer('StartDelay', 0.05, 'TimerFcn', 'global timerDrainCount; timerDrainCount = timerDrainCount + 1;');
end
for k = 1:4
  start(t(k));
end
tStart = tic();
while toc(tStart) < 1
end
before = timerDrainCount;
drawnow();
asserts.isequal(timerDrainCount, 4);
asserts.istrue(before < 4);
for k = 1:4
  stop(t(k));
  delete(t(k));
end
close(figs);
%=============================================================================
function countCall(source, event)
  source.UserData = source.UserData + 1;
end
%=============================================================================
function requeue(source, handle)
  source.UserData = source.UserData + 1;
  if source.UserData < 10
    __web_figure_size__(handle, 650 + source.UserData, 500);
  end
end
%=============================================================================
