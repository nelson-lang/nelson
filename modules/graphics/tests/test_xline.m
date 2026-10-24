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
h = xline(1);
asserts.isequal(h.Type, 'constantline');
asserts.isequal(class(h), 'nelson.graphics.chart.decoration.ConstantLine');
asserts.isequal(h.Value, 1);
asserts.isequal(h.InterceptAxis, 'x');
asserts.isequal(h.Color, grey);
asserts.isequal(h.LabelColor, grey);
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.LineWidth, 0.5);
asserts.isequal(h.Alpha, 0.7);
asserts.isequal(h.Layer, 'top');
asserts.isequal(h.Label, '');
asserts.isequal(h.FontName, 'Helvetica');
asserts.isequal(h.FontSize, 10);
asserts.isequal(h.FontAngle, 'normal');
asserts.isequal(h.FontWeight, 'normal');
asserts.isequal(h.Interpreter, 'tex');
asserts.isequal(h.LabelOrientation, 'aligned');
asserts.isequal(h.LabelHorizontalAlignment, 'right');
asserts.isequal(h.LabelVerticalAlignment, 'top');
asserts.isequal(h.ColorMode, 'auto');
asserts.isequal(h.LabelColorMode, 'auto');
asserts.isequal(h.LineStyleMode, 'auto');
asserts.isequal(h.SeriesIndex, 'none');
delete(h);
close(f);
%=============================================================================
% The ConstantLine surfaces exactly the reference property set (41 properties).
f = figure('Visible', 'off');
h = xline(1);
names = sort(fieldnames(get(h)));
expected = sort({'Value'; 'InterceptAxis'; 'Label'; 'FontAngle'; 'FontSize'; 'FontName'; ...
  'FontWeight'; 'Interpreter'; 'LabelOrientation'; 'LabelHorizontalAlignment'; ...
  'LabelVerticalAlignment'; 'Alpha'; 'LineWidth'; 'Color'; 'LabelColor'; 'LineStyle'; ...
  'Layer'; 'ColorMode'; 'LabelColorMode'; 'LineStyleMode'; 'Children'; 'Parent'; 'Visible'; ...
  'HandleVisibility'; 'DisplayName'; 'Annotation'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; ...
  'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; ...
  'Selected'; 'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'SeriesIndex'});
asserts.isequal(names, expected);
delete(h);
close(f);
%=============================================================================
% Value, Color and LineStyle are settable.
f = figure('Visible', 'off');
h = xline(3, 'Color', [1 0 0], 'LineWidth', 2, 'LineStyle', ':');
asserts.isequal(h.Value, 3);
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.LineStyle, ':');
h.Value = 5;
asserts.isequal(h.Value, 5);
h.Color = [0 1 0];
asserts.isequal(h.Color, [0 1 0]);
delete(h);
close(f);
%=============================================================================
% A LineSpec sets the color and line style.
f = figure('Visible', 'off');
h = xline(2, '--r');
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.Color, [1 0 0]);
delete(h);
close(f);
%=============================================================================
% A LineSpec followed by a label.
f = figure('Visible', 'off');
h = xline(2, '-.b', 'threshold');
asserts.isequal(h.LineStyle, '-.');
asserts.isequal(h.Color, [0 0 1]);
asserts.isequal(h.Label, 'threshold');
delete(h);
close(f);
%=============================================================================
% Label passed as a Name/Value pair.
f = figure('Visible', 'off');
h = xline(1, 'Label', 'limit', 'LabelHorizontalAlignment', 'left');
asserts.isequal(h.Label, 'limit');
asserts.isequal(h.LabelHorizontalAlignment, 'left');
delete(h);
close(f);
%=============================================================================
% A vector of values creates one line per value.
f = figure('Visible', 'off');
h = xline([1 2 3]);
asserts.isequal(numel(h), 3);
asserts.isequal(h(1).Value, 1);
asserts.isequal(h(2).Value, 2);
asserts.isequal(h(3).Value, 3);
delete(h);
close(f);
%=============================================================================
% The line attaches to the current axes.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = xline(1);
asserts.isequal(h.Parent, ax);
delete(h);
close(f);
%=============================================================================
% InterceptAxis is restricted to 'x' or 'y'.
f = figure('Visible', 'off');
h = xline(1);
failed = false;
try
  h.InterceptAxis = 'z';
catch
  failed = true;
end
asserts.istrue(failed);
delete(h);
close(f);
%=============================================================================
% xline requires at least one argument.
failed = false;
try
  xline();
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
% The x-axis autoscales to include the constant value; the y-axis is untouched.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
ylimBefore = ax.YLim;
xline(15);
xl = xlim(ax);
asserts.istrue(xl(1) <= 15 && 15 <= xl(2));
asserts.isequal(ax.YLim, ylimBefore);
close(f);
%=============================================================================
% A value inside the current data range does not change the x-limits.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10, 1:10);
xlimBefore = ax.XLim;
xline(5);
asserts.isequal(ax.XLim, xlimBefore);
close(f);
%=============================================================================
% An xline alone puts its value inside the x-limits (default y-limits kept).
f = figure('Visible', 'off');
ax = axes('Parent', f);
xline(7);
xl = xlim(ax);
asserts.istrue(xl(1) <= 7 && 7 <= xl(2));
close(f);
%=============================================================================
