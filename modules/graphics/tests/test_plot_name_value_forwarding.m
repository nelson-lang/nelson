%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 6-->
% <--ADV-CLI MODE-->
%=============================================================================
% A name/value pair given to a plotting function must reach the object it
% creates. 'Tag' is the probe: every graphics object has one and no plotting
% function sets it, so a Tag that did not land means the pair was dropped on
% the way (or the property name was taken for data).
%
% meshz is left out on purpose: it keeps the Tag given by the caller, and that
% is what this file would otherwise pin down twice.
%=============================================================================
statements = {...
  'h = plot([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = plot3([1 2 3], [1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = line([1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = semilogx([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = semilogy([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = loglog([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = bar([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = barh([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = stem([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = stairs([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = area([1 2 3], ''Tag'', ''PROBE'');', ...
  'h = errorbar([1 2 3], [1 2 3], [0.1 0.1 0.1], ''Tag'', ''PROBE'');', ...
  'h = scatter([1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = scatter3([1 2 3], [1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = stem3([1 2 3], [1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = quiver([1 2], [1 2], [1 1], [1 1], ''Tag'', ''PROBE'');', ...
  'h = quiver3([1 2], [1 2], [1 2], [1 1], [1 1], [1 1], ''Tag'', ''PROBE'');', ...
  'h = surface(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = surf(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = surfc(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = surfl(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = mesh(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = meshc(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = waterfall(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = ribbon(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = pcolor(peaks(5), ''Tag'', ''PROBE'');', ...
  '[~, h] = contour(peaks(5), ''Tag'', ''PROBE'');', ...
  '[~, h] = contourf(peaks(5), ''Tag'', ''PROBE'');', ...
  '[~, h] = contour3(peaks(5), ''Tag'', ''PROBE'');', ...
  'h = histogram([1 2 2 3 3 3], ''Tag'', ''PROBE'');', ...
  'h = histogram2([1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = binscatter([1 2 3], [1 2 3], ''Tag'', ''PROBE'');', ...
  'h = fill([0 1 1 0], [0 0 1 1], ''r'', ''Tag'', ''PROBE'');', ...
  'h = patch(''XData'', [0 1 1], ''YData'', [0 0 1], ''Tag'', ''PROBE'');', ...
  'h = image(magic(4), ''Tag'', ''PROBE'');', ...
  'h = imagesc(magic(4), ''Tag'', ''PROBE'');', ...
  'h = text(0.5, 0.5, ''x'', ''Tag'', ''PROBE'');', ...
  'h = rectangle(''Position'', [0 0 1 1], ''Tag'', ''PROBE'');', ...
  'h = fplot(@(x) x, ''Tag'', ''PROBE'');', ...
  'h = fsurf(@(x, y) x + y, [0 1 0 1], ''Tag'', ''PROBE'');', ...
  'h = fmesh(@(x, y) x + y, [0 1 0 1], ''Tag'', ''PROBE'');', ...
  'h = fcontour(@(x, y) x + y, [0 1 0 1], ''Tag'', ''PROBE'');', ...
  'h = polarplot([0 1 2], [1 2 3], ''Tag'', ''PROBE'');'};
%=============================================================================
for k = 1:numel(statements)
  statement = statements{k};
  f = figure('Visible', 'off');
  eval(statement);
  assert_isequal(get(h(1), 'Tag'), 'PROBE', ...
    [statement, ' must set the Tag it was given.']);
  close(f);
end
%=============================================================================
% ribbon draws one surface per column and every one of them takes the pairs.
f = figure('Visible', 'off');
h = ribbon(peaks(5), 'Tag', 'PROBE');
assert_isequal(numel(h), 5);
for k = 1:numel(h)
  assert_isequal(get(h(k), 'Tag'), 'PROBE');
end
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = ribbon(peaks(5), 'EdgeColor', 'r', 'FaceAlpha', 0.5);
assert_isequal(h(1).EdgeColor, [1 0 0]);
assert_isequal(h(1).FaceAlpha, 0.5);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = ribbon(1:5, peaks(5), 'LineWidth', 3);
assert_isequal(h(1).LineWidth, 3);
close(f);
%=============================================================================
% pcolor applies its dark edge colour as a default, so a caller EdgeColor wins.
f = figure('Visible', 'off');
h = pcolor(peaks(5), 'EdgeColor', 'r');
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = pcolor(peaks(5));
% The pcolor default edge is the dark-gray axes foreground (33/255).
assert_isequal(h.EdgeColor, [0.129411764705882, 0.129411764705882, 0.129411764705882]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = pcolor(1:5, 1:5, peaks(5), 'FaceColor', 'g', 'LineWidth', 3);
assert_isequal(h.FaceColor, [0 1 0]);
assert_isequal(h.LineWidth, 3);
close(f);
%=============================================================================
% stem owns the pairs given to it: they land on the stem, not on the lines
% drawn for it, and a stem property is no longer taken for a line spec.
f = figure('Visible', 'off');
h = stem([1 2 3], 'Tag', 'PROBE', 'DisplayName', 'DN', 'UserData', 42);
assert_isequal(h.Tag, 'PROBE');
assert_isequal(h.DisplayName, 'DN');
assert_isequal(h.UserData, 42);
assert_isequal(numel(findobj(f, 'Tag', 'PROBE')), 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem([1 2 3], 'BaseValue', 1);
assert_isequal(h.BaseValue, 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem([1 2 3], 'ShowBaseLine', 'off');
assert_isequal(char(h.ShowBaseLine), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem([1 2 3], 'HandleVisibility', 'off');
assert_isequal(char(h.HandleVisibility), 'off');
close(f);
%=============================================================================
% The line spec still reaches stem, and so do the line properties.
f = figure('Visible', 'off');
h = stem([1 2 3], 'r--s');
assert_isequal(h.Color, [1 0 0]);
assert_isequal(h.Marker, 'square');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem([1 2 3], 'LineWidth', 3, 'MarkerSize', 12);
assert_isequal(h.LineWidth, 3);
assert_isequal(h.MarkerSize, 12);
close(f);
%=============================================================================
% pcolor and ribbon start the new plot in the axes named by a 'Parent' pair.
parentPropertyStatements = {...
  'pcolor(peaks(5), ''Parent'', ax1)', ...
  'pcolor(1:5, 1:5, peaks(5), ''Parent'', ax1)', ...
  'ribbon(peaks(5), ''Parent'', ax1)', ...
  'ribbon(1:5, peaks(5), ''Parent'', ax1)', ...
  'stem([1 2 3], ''Parent'', ax1)'};
for k = 1:numel(parentPropertyStatements)
  statement = parentPropertyStatements{k};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  line('Parent', ax2, 'XData', [0 1], 'YData', [0 1], 'Tag', 'BYSTANDERW');
  eval(statement);
  assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')), ...
    [statement, ' must clear the axes named by Parent.']);
  assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1, ...
    [statement, ' must not touch the current axes.']);
  assert_istrue(numel(ax1.Children) >= 1, ...
    [statement, ' must draw in the axes named by Parent.']);
  assert_isequal(gca(), ax2, [statement, ' must not change the current axes.']);
  close(f);
end
%=============================================================================
% A group named by Parent resolves to the axes holding it: neither the cells
% nor the ribbons can be a child of a group.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
g = hggroup('Parent', ax1);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
h = pcolor(peaks(5), 'Parent', g);
assert_isequal(h.Parent, ax1);
assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
g = hggroup('Parent', ax1);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
h = ribbon(peaks(5), 'Parent', g);
assert_isequal(h(1).Parent, ax1);
assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')));
close(f);
%=============================================================================
