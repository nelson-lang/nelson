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
% A value trailing the bin specification counts for nothing: the reference
% implementation drops whatever follows the bins, so hist(Y, X, 2) hands back
% the same counts and draws the same histogram as hist(Y, X).
%
% Such a value must not be forwarded either: bar has no positional left for it
% and would reject the whole call.
%
% Property arguments are still forwarded, so a numeric standing there as the
% VALUE of a pair has to survive. Only the arguments sitting before the first
% property name are dropped.
%=============================================================================
Y = [1 2 2 3 3 3];
EXPECTED_COUNTS = [1 2 3];
EXPECTED_CENTERS = [1 2 3];
%=============================================================================
% The counts never depend on what trails the bin specification.
[counts, centers] = hist(Y, [1 2 3], 2);
assert_isequal(counts, EXPECTED_COUNTS);
assert_isequal(centers, EXPECTED_CENTERS);
%=============================================================================
counts = hist(Y, [1 2 3], 2, 3);
assert_isequal(counts, EXPECTED_COUNTS);
%=============================================================================
[counts, centers] = hist([1 2; 2 3; 3 3], [1 2 3], 2);
assert_isequal(counts, [1 0; 1 1; 1 2]);
assert_isequal(centers, [1; 2; 3]);
%=============================================================================
% A trailing value draws exactly the histogram drawn without it.
f = figure('Visible', 'off');
hist(Y, [1 2 3]);
ax = gca();
witness = ax.Children(1);
PLAIN_XDATA = witness.XData;
PLAIN_YDATA = witness.YData;
PLAIN_BARWIDTH = witness.BarWidth;
close(f);
%=============================================================================
statements = {...
  'hist(Y, [1 2 3], 2)', ...
  'hist(Y, [1 2 3], 0.25)', ...
  'hist(Y, [1 2 3], 2, 3)'};
for k = 1:numel(statements)
  statement = statements{k};
  f = figure('Visible', 'off');
  eval([statement, ';']);
  ax = gca();
  drawn = ax.Children(1);
  assert_isequal(drawn.XData, PLAIN_XDATA, ...
    [statement, ' must draw the same bins.']);
  assert_isequal(drawn.YData, PLAIN_YDATA, ...
    [statement, ' must draw the same counts.']);
  assert_isequal(drawn.BarWidth, PLAIN_BARWIDTH, ...
    [statement, ' must leave the bar width alone.']);
  close(f);
end
%=============================================================================
% The same holds when the bins are given as a count.
f = figure('Visible', 'off');
hist(Y, 3);
ax = gca();
witness = ax.Children(1);
COUNTED_XDATA = witness.XData;
COUNTED_YDATA = witness.YData;
close(f);
%=============================================================================
f = figure('Visible', 'off');
hist(Y, 3, 2);
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.XData, COUNTED_XDATA);
assert_isequal(drawn.YData, COUNTED_YDATA);
close(f);
%=============================================================================
% Property arguments are honoured, whether or not a dropped value precedes them.
styles = {...
  'hist(Y, [1 2 3], ''FaceColor'', ''r'')', ...
  'hist(Y, [1 2 3], 2, ''FaceColor'', ''r'')', ...
  'hist(Y, [1 2 3], ''FaceColor'', ''r'', ''EdgeColor'', ''g'')', ...
  'hist(Y, [1 2 3], ''r'')'};
for k = 1:numel(styles)
  statement = styles{k};
  f = figure('Visible', 'off');
  eval([statement, ';']);
  ax = gca();
  drawn = ax.Children(1);
  assert_isequal(drawn.FaceColor, [1 0 0], ...
    [statement, ' must paint the bars as asked.']);
  close(f);
end
%=============================================================================
% A numeric standing as the value of a pair is not a trailing value.
f = figure('Visible', 'off');
hist(Y, [1 2 3], 'LineWidth', 3);
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.LineWidth, 3);
close(f);
%=============================================================================
f = figure('Visible', 'off');
hist(Y, [1 2 3], 2, 'LineWidth', 3);
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.LineWidth, 3);
close(f);
%=============================================================================
% The designated target still wins with a value trailing the bins.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
hist(Y, [1 2 3], 2, 'Parent', ax1);
assert_istrue(numel(ax1.Children) >= 1, ...
  'hist must draw in the axes it was given.');
assert_isequal(numel(ax2.Children), 0, ...
  'hist must not draw in the current axes.');
close(f);
%=============================================================================
