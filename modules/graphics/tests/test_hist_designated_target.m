%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% hist() must take the target it was handed out of the way of its data
% arguments whatever nargout is. Asked for its counts it draws nothing, but the
% leading axes still has to be consumed instead of being read as the sample.
%
% The witness is found back by Tag and never by handle: a freed handle id is
% handed out again to the next object created, so isgraphics() on a stale handle
% answers for whatever object now holds that id.
%=============================================================================
Y = [1 2 2 3 3 3];
EXPECTED_COUNTS = [1 0 0 0 2 0 0 0 0 3];
EXPECTED_CENTERS = [1.1 1.3 1.5 1.7 1.9 2.1 2.3 2.5 2.7 2.9];
%=============================================================================
% Drawing form: the designated axes is cleared, and only it.
statements = {...
  'hist(ax1, Y)', ...
  'hist(ax1, Y, 3)', ...
  'hist(ax1, Y, [1 2 3])', ...
  'hist(Y, ''Parent'', ax1)', ...
  'hist(Y, 3, ''Parent'', ax1)'};
for k = 1:numel(statements)
  statement = statements{k};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  line('Parent', ax2, 'XData', [0 1], 'YData', [0 1], 'Tag', 'BYSTANDERW');
  eval(statement);
  assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')), ...
    [statement, ' must clear the axes it was given.']);
  assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1, ...
    [statement, ' must not touch the current axes.']);
  assert_istrue(numel(ax1.Children) >= 1, ...
    [statement, ' must draw in the axes it was given.']);
  assert_isequal(gca(), ax2, [statement, ' must not change the current axes.']);
  close(f);
end
%=============================================================================
% NextPlot 'add' (hold on) keeps the existing children in place.
for k = 1:numel(statements)
  statement = statements{k};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
  set(ax1, 'NextPlot', 'add');
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  eval(statement);
  assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1, ...
    [statement, ' must not clear the axes while NextPlot is add.']);
  close(f);
end
%=============================================================================
% Counting form: the target is consumed, nothing is drawn, nothing is cleared.
countingStatements = {...
  'counts = hist(ax1, Y)', ...
  'counts = hist(Y, ''Parent'', ax1)'};
for k = 1:numel(countingStatements)
  statement = countingStatements{k};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  line('Parent', ax2, 'XData', [0 1], 'YData', [0 1], 'Tag', 'BYSTANDERW');
  counts = [];
  eval([statement, ';']);
  assert_isequal(counts, EXPECTED_COUNTS, ...
    [statement, ' must count the sample, not the axes.']);
  assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1, ...
    [statement, ' must not clear the axes it was given.']);
  assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1, ...
    [statement, ' must not touch the current axes.']);
  assert_isequal(numel(ax1.Children), 1, [statement, ' must not draw.']);
  assert_isequal(gca(), ax2, [statement, ' must not change the current axes.']);
  close(f);
end
%=============================================================================
% Two outputs, same contract.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
[counts, centers] = hist(ax1, Y);
assert_isequal(counts, EXPECTED_COUNTS);
assert_isapprox(centers, EXPECTED_CENTERS, 1e-10);
assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1);
assert_isequal(numel(ax1.Children), 1);
close(f);
%=============================================================================
% A designated target changes nothing to the counts.
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
[countsWithTarget, centersWithTarget] = hist(ax1, Y, 3);
[countsAlone, centersAlone] = hist(Y, 3);
assert_isequal(countsWithTarget, countsAlone);
assert_isequal(centersWithTarget, centersAlone);
close(f);
%=============================================================================
% An hggroup target adds to the group and leaves the axes holding it alone.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
g = hggroup('Parent', ax1);
hist(g, Y);
assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1);
assert_istrue(numel(g.Children) >= 1);
close(f);
%=============================================================================
% A target on its own carries no sample.
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
assert_checkerror('hist(ax1)', getString(message('Nelson:notEnoughInputs')), ...
  'Nelson:notEnoughInputs');
close(f);
%=============================================================================
