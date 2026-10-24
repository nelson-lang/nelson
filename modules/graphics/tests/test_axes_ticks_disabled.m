%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
reset(groot());
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
plot(ax, 1:10);
set(ax, 'XGrid', 'on', 'YGrid', 'on', ...
  'XMinorGrid', 'on', 'YMinorGrid', 'on', ...
  'XMinorTick', 'on', 'YMinorTick', 'on');
set(ax, 'XTick', [], 'YTick', []);
drawnow();
asserts.isequal(ax.XTickMode, 'manual');
asserts.isequal(ax.YTickMode, 'manual');
asserts.istrue(isempty(ax.XTick));
asserts.istrue(isempty(ax.YTick));
asserts.isequal(ax.XTick, []);
asserts.isequal(ax.YTick, []);
asserts.istrue(isempty(ax.XTickLabel));
asserts.istrue(isempty(ax.YTickLabel));
frame = getframe(f);
asserts.isequal(size(frame.cdata), expectedFrameSize(f));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10);
set(ax, 'XTick', [-10 2 4 20], 'YTick', [0 10 50 100]);
drawnow();
asserts.isequal(ax.XTick, [-10 2 4 20]);
asserts.isequal(ax.YTick, [0 10 50 100]);
asserts.isequal(ax.XTickLabel, {'2'; '4'});
asserts.isequal(ax.YTickLabel, {'10'});
close(f);
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
semilogx(ax, 1:10, 1:10);
set(ax, 'XMinorTick', 'on', 'XMinorGrid', 'on', 'XGrid', 'on');
set(ax, 'XTick', []);
drawnow();
asserts.isequal(ax.XTickMode, 'manual');
asserts.istrue(isempty(ax.XTick));
asserts.istrue(isempty(ax.XTickLabel));
frame = getframe(f);
asserts.isequal(size(frame.cdata), expectedFrameSize(f));
close(f);
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
[x, y] = meshgrid(1:4, 1:4);
surf(ax, x, y, x + y);
view(ax, 3);
set(ax, 'ZMinorTick', 'on', 'ZMinorGrid', 'on', 'ZGrid', 'on', 'ZTick', []);
drawnow();
asserts.isequal(ax.ZTickMode, 'manual');
asserts.istrue(isempty(ax.ZTick));
asserts.isequal(ax.ZTick, []);
asserts.istrue(isempty(ax.ZTickLabel));
frame = getframe(f);
asserts.isequal(size(frame.cdata), expectedFrameSize(f));
close(f);
%=============================================================================
close all
%=============================================================================
function frameSize = expectedFrameSize(f)
  pos = f.Position;
  % getframe captures a figure that contains axes at device resolution.
  ratio = f.DevicePixelRatio;
  frameSize = [round(pos(4) * ratio), round(pos(3) * ratio), 3];
end
%=============================================================================
