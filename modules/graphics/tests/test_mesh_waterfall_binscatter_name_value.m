%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% mesh, meshz, meshc, waterfall and binscatter must take name/value pairs: the
% pair must reach the object created, a pair given by the caller must win over
% the default the function applies, and a 'Parent' pair must designate the
% target without leaving a duplicate behind for the low level constructor.
%
% The witness is found back by Tag and never by handle: a freed handle id is
% handed out again to the next object created, so isgraphics() on a stale handle
% answers for whatever object now holds that id.
%=============================================================================
parentPropertyStatements = {...
  'mesh(peaks(5), ''Parent'', ax1)', ...
  'mesh(1:5, 1:5, peaks(5), ''Parent'', ax1)', ...
  'mesh(peaks(5), ''EdgeColor'', ''r'', ''Parent'', ax1)', ...
  'meshz(peaks(5), ''Parent'', ax1)', ...
  'meshz(1:5, 1:5, peaks(5), ''Parent'', ax1)', ...
  'meshz(peaks(5), ''EdgeColor'', ''r'', ''Parent'', ax1)', ...
  'waterfall(peaks(5), ''Parent'', ax1)', ...
  'waterfall(1:5, 1:5, peaks(5), ''Parent'', ax1)', ...
  'waterfall(peaks(5), ''EdgeColor'', ''r'', ''Parent'', ax1)', ...
  'binscatter([1 2 3], [1 2 3], ''Parent'', ax1)', ...
  'binscatter([1 2 3], [1 2 3], 4, ''Parent'', ax1)', ...
  'binscatter([1 2 3], [1 2 3], ''NumBins'', [2 2], ''Parent'', ax1)'};
%=============================================================================
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
% A property given by the caller wins over the default the function applies.
f = figure('Visible', 'off');
h = mesh(peaks(5), 'EdgeColor', 'r');
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = mesh(peaks(5), 'FaceColor', 'g');
assert_isequal(h.FaceColor, [0 1 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = mesh(peaks(5), 'FaceLighting', 'gouraud', 'LineWidth', 2, 'LineStyle', ':');
assert_isequal(h.FaceLighting, 'gouraud');
assert_isequal(h.LineWidth, 2);
assert_isequal(h.LineStyle, ':');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = meshz(peaks(5), 'EdgeColor', 'r');
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = meshz(peaks(5), 'FaceColor', 'g');
assert_isequal(h.FaceColor, [0 1 0]);
close(f);
%=============================================================================
% meshc returns the surface and the contour, the surface first.
f = figure('Visible', 'off');
h = meshc(peaks(5), 'EdgeColor', 'r');
assert_isequal(h(1).EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = meshc(peaks(5), 'FaceColor', 'g', 'LineWidth', 2);
assert_isequal(h(1).FaceColor, [0 1 0]);
assert_isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================
% waterfall paints the curtains with the colormap and keeps a white fill, so an
% EdgeColor given by the caller wins while FaceColor stays the waterfall one.
f = figure('Visible', 'off');
h = waterfall(peaks(5), 'EdgeColor', 'r');
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = waterfall(peaks(5), 'FaceColor', 'g', 'LineWidth', 2);
assert_isequal(h.FaceColor, [1 1 1]);
assert_isequal(h.LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = binscatter([1 2 3], [1 2 3], 'FaceAlpha', 0.5, 'ShowEmptyBins', 'on');
assert_isequal(h.FaceAlpha, 0.5);
assert_isequal(char(h.ShowEmptyBins), 'on');
close(f);
%=============================================================================
% Names and values given as strings are taken just like char ones.
f = figure('Visible', 'off');
h = mesh(peaks(5), "EdgeColor", "r");
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = meshz(peaks(5), "EdgeColor", "r");
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = waterfall(peaks(5), "EdgeColor", "r");
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
% Repeating a property is legal for a patch: the last value wins, as it already
% does for a surface. waterfall leans on it to let its defaults be overridden.
f = figure('Visible', 'off');
h = patch('XData', [0 1 1], 'YData', [0 0 1], 'LineWidth', 1, 'LineWidth', 3);
assert_isequal(h.LineWidth, 3);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = patch('XData', [0 1 1], 'YData', [0 0 1], 'EdgeColor', 'flat', 'EdgeColor', 'r');
assert_isequal(h.EdgeColor, [1 0 0]);
close(f);
%=============================================================================
% A 'Parent' pair given twice keeps the last one, as a repeated property does.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
h = mesh(peaks(5), 'Parent', ax1, 'Parent', ax2);
assert_isequal(h.Parent, ax2);
close(f);
%=============================================================================
% An hggroup named by Parent: mesh adds to the group and leaves the axes that
% holds it alone, waterfall and binscatter draw in that axes and start it anew.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
g = hggroup('Parent', ax1);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
h = mesh(peaks(5), 'Parent', g);
assert_isequal(h.Parent, g);
assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
g = hggroup('Parent', ax1);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
h = waterfall(peaks(5), 'Parent', g);
assert_isequal(h.Parent, ax1);
assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
g = hggroup('Parent', ax1);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
h = binscatter([1 2 3], [1 2 3], 'Parent', g);
assert_isequal(h.Parent, ax1);
assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')));
close(f);
%=============================================================================
