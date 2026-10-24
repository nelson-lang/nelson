%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
grey = [0.129411764705882 0.129411764705882 0.129411764705882];
%=============================================================================
f = figure('Visible', 'off');
h = yline(1);
asserts.isequal(h.Type, 'constantline');
asserts.isequal(class(h), 'nelson.graphics.chart.decoration.ConstantLine');
asserts.isequal(h.Value, 1);
asserts.isequal(h.InterceptAxis, 'y');
asserts.isequal(h.Color, grey);
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.LineWidth, 0.5);
asserts.isequal(h.Alpha, 0.7);
asserts.isequal(h.Label, '');
delete(h);
close(f);
%=============================================================================
% yline and xline share the ConstantLine class but differ on InterceptAxis.
f = figure('Visible', 'off');
hy = yline(2);
hx = xline(2);
asserts.isequal(hy.InterceptAxis, 'y');
asserts.isequal(hx.InterceptAxis, 'x');
asserts.isequal(class(hy), class(hx));
delete(hy);
delete(hx);
close(f);
%=============================================================================
% Value, Color and LineStyle are settable.
f = figure('Visible', 'off');
h = yline(3, 'Color', [0 0 1], 'LineStyle', '--', 'Label', 'mean');
asserts.isequal(h.Value, 3);
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Label, 'mean');
h.Value = -4;
asserts.isequal(h.Value, -4);
delete(h);
close(f);
%=============================================================================
% A LineSpec sets the color and line style.
f = figure('Visible', 'off');
h = yline(2, ':g');
asserts.isequal(h.LineStyle, ':');
asserts.isequal(h.Color, [0 1 0]);
delete(h);
close(f);
%=============================================================================
% A LineSpec followed by a label (same argument handling as xline).
f = figure('Visible', 'off');
h = yline(2, '-.b', 'threshold');
asserts.isequal(h.LineStyle, '-.');
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.Label, 'threshold');
delete(h);
close(f);
%=============================================================================
% Label passed as a Name/Value pair (same argument handling as xline).
f = figure('Visible', 'off');
h = yline(1, 'Label', 'limit', 'LabelHorizontalAlignment', 'left');
asserts.isequal(h.Label, 'limit');
asserts.isequal(h.LabelHorizontalAlignment, 'left');
delete(h);
close(f);
%=============================================================================
% A vector of values creates one line per value.
f = figure('Visible', 'off');
h = yline([0 10]);
asserts.isequal(numel(h), 2);
asserts.isequal(h(1).Value, 0);
asserts.isequal(h(2).Value, 10);
delete(h);
close(f);
%=============================================================================
% yline requires at least one argument.
failed = false;
try
  yline();
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
% The y-axis autoscales to include the constant value; the x-axis is untouched.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
xlimBefore = ax.XLim;
yline(15);
yl = ylim(ax);
asserts.istrue(yl(1) <= 15 && 15 <= yl(2));
asserts.isequal(ax.XLim, xlimBefore);
close(f);
%=============================================================================
% A value inside the current data range does not change the y-limits.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
ylimBefore = ax.YLim;
yline(5);
asserts.isequal(ax.YLim, ylimBefore);
close(f);
%=============================================================================
