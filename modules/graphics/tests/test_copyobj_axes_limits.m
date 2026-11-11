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
close all
%=============================================================================
% Limits not read yet on the source are computed before the copy.
f = figure('Visible', 'off');
ax = axes(f);
plot(ax, 1:5);
f2 = figure('Visible', 'off');
ax2 = copyobj(ax, f2);
asserts.isequal(ax2.XLim, [1 5]);
asserts.isequal(ax.XLim, [1 5]);
%=============================================================================
% Auto modes stay auto on the copy.
modes = {'XLimMode', 'YLimMode', 'ZLimMode', 'CLimMode', 'XTickMode', ...
  'XTickLabelMode', 'FontSizeMode', 'DataAspectRatioMode', 'TickDirMode'};
for k = 1:numel(modes)
  asserts.isequal(ax2.(modes{k}), 'auto');
end
asserts.isequal(ax2.PositionConstraint, 'outerposition');
%=============================================================================
% Auto limits of the copy follow its new data.
plot(ax2, 1:20);
asserts.isequal(ax2.XLim, [0 20]);
%=============================================================================
% Manual modes of the source are kept, auto ones are not flipped.
ax.XLim = [0 3];
ax3 = copyobj(ax, f2);
asserts.isequal(ax3.XLim, [0 3]);
asserts.isequal(ax3.XLimMode, 'manual');
asserts.isequal(ax3.YLimMode, 'auto');
asserts.isequal(ax3.XTickMode, 'auto');
ax.XTick = [0 1 2 3];
ax.YLimMode = 'manual';
ax4 = copyobj(ax, f2);
asserts.isequal(ax4.XTick, [0 1 2 3]);
asserts.isequal(ax4.XTickMode, 'manual');
asserts.isequal(ax4.YLimMode, 'manual');
asserts.isequal(ax4.XTickLabelMode, 'auto');
%=============================================================================
% Color limits of an image copy stay auto.
f3 = figure('Visible', 'off');
a = axes(f3);
imagesc(a, magic(4));
c = copyobj(a, f3);
asserts.isequal(c.CLim, [1 16]);
asserts.isequal(c.CLimMode, 'auto');
%=============================================================================
close all
%=============================================================================
