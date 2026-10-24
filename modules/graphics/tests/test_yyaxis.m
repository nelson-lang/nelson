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
% yyaxis turns an axes into a two-sided one: two y-rulers sharing a single x
% axis, each with its own limits, colour and children. The active side is the
% one exposed through the canonical Y* properties. Behaviour is verified
% against the reference.
%=============================================================================
f = figure('Visible', 'off');
yyaxis left
h1 = plot([1 2 3], [2 6 4]);
leftColor = get(gca, 'YColor');
leftLim = get(gca, 'YLim');
leftLine = get(h1, 'Color');
asserts.isequal(get(gca, 'YAxisLocation'), 'left');
%=============================================================================
yyaxis right
h2 = plot([1 2 3], [500 300 700]);
rightColor = get(gca, 'YColor');
rightLim = get(gca, 'YLim');
rightLine = get(h2, 'Color');
% After switching, the active side is the right one.
asserts.isequal(get(gca, 'YAxisLocation'), 'right');
%=============================================================================
% The two rulers take the first two ColorOrder entries.
colorOrder = get(gca, 'ColorOrder');
asserts.isapprox(leftColor, colorOrder(1, :), 1e-6);
asserts.isapprox(rightColor, colorOrder(2, :), 1e-6);
% The first line of each side matches its ruler colour.
asserts.isapprox(leftLine, colorOrder(1, :), 1e-6);
asserts.isapprox(rightLine, colorOrder(2, :), 1e-6);
%=============================================================================
% Each side keeps its own, independent limits.
asserts.isapprox(leftLim, [2 6], 1e-6);
asserts.isapprox(rightLim, [300 700], 1e-6);
% Children only report the active side.
asserts.isequal(numel(get(gca, 'Children')), 1);
%=============================================================================
% Switching back restores the left side's state exactly.
yyaxis left
asserts.isequal(get(gca, 'YAxisLocation'), 'left');
asserts.isapprox(get(gca, 'YColor'), colorOrder(1, :), 1e-6);
asserts.isapprox(get(gca, 'YLim'), [2 6], 1e-6);
asserts.isequal(numel(get(gca, 'Children')), 1);
%=============================================================================
% The x axis is shared: setting it from either side is visible from the other.
xlim([0 5]);
asserts.isapprox(get(gca, 'XLim'), [0 5], 1e-6);
yyaxis right
asserts.isapprox(get(gca, 'XLim'), [0 5], 1e-6);
%=============================================================================
close(f);
%=============================================================================
% yyaxis on an empty current axes creates one and activates the requested side.
f = figure('Visible', 'off');
yyaxis right
asserts.isequal(get(gca, 'YAxisLocation'), 'right');
close(f);
%=============================================================================
% Explicit axes target form: yyaxis(ax, side).
f = figure('Visible', 'off');
ax = axes('Parent', f);
yyaxis(ax, 'right');
asserts.isequal(get(ax, 'YAxisLocation'), 'right');
yyaxis(ax, 'left');
asserts.isequal(get(ax, 'YAxisLocation'), 'left');
close(f);
%=============================================================================
% Rendering a two-sided axes to an image must succeed.
f = figure('Visible', 'off');
yyaxis left
plot([1 2 3], [2 6 4]);
yyaxis right
plot([1 2 3], [500 300 700]);
imageFile = [tempname(), '.png'];
saveas(f, imageFile);
asserts.istrue(isfile(imageFile));
if isfile(imageFile)
  delete(imageFile);
end
close(f);
%=============================================================================
% Side argument validation.
f = figure('Visible', 'off');
assert_checkerror('yyaxis(''middle'')', getString(message('Nelson:yyaxis:invalidSide')));
close(f);
%=============================================================================
% Axes.YAxis exposes the two rulers as NumericRuler objects. YAxis(1) is the
% left side and YAxis(2) the right, whatever the active side.
f = figure('Visible', 'off');
yyaxis left
plot([1 2 3], [2 6 4]);
yyaxis right
plot([1 2 3], [500 300 700]);
ax = gca();
ya = ax.YAxis;
asserts.isequal(numel(ya), 2);
asserts.isequal(class(ya(1)), 'nelson.graphics.axis.decorator.NumericRuler');
asserts.isapprox(ya(1).Limits, [2 6], 1e-6);
asserts.isapprox(ya(2).Limits, [300 700], 1e-6);
%=============================================================================
% Editing a ruler writes back to the matching side (by reference).
ax.YAxis(2).Limits = [0 1000];
asserts.isapprox(get(ax, 'YLim'), [0 1000], 1e-6);
ax.YAxis(1).Color = [0 1 0];
yyaxis left
asserts.isapprox(get(ax, 'YColor'), [0 1 0], 1e-6);
close(f);
%=============================================================================
% A plain axes exposes a single ruler.
f = figure('Visible', 'off');
plot([1 2 3], [2 6 4]);
asserts.isequal(numel(get(gca, 'YAxis')), 1);
close(f);
%=============================================================================
% cla reset removes the second y-axis.
f = figure('Visible', 'off');
yyaxis left
plot([1 2 3], [2 6 4]);
yyaxis right
plot([1 2 3], [500 300 700]);
cla reset
asserts.isequal(numel(get(gca, 'YAxis')), 1);
asserts.isequal(get(gca, 'YAxisLocation'), 'left');
close(f);
%=============================================================================
% hold applies to both sides.
f = figure('Visible', 'off');
yyaxis left
plot([1 2 3], [2 6 4]);
hold on
plot([1 2 3], [1 2 3]);
asserts.isequal(numel(get(gca, 'Children')), 2);
yyaxis right
plot([1 2 3], [500 300 700]);
plot([1 2 3], [100 200 150]);
asserts.isequal(numel(get(gca, 'Children')), 2);
close(f);
%=============================================================================
