%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% The legacy hist draws its bars in a dark indigo blue of its own, distinct
% from the ColorOrder blue a plain bar uses, and rounds the position axis out
% onto round numbers instead of hugging the samples.
HIST_FACE_COLOR = [0.2 0.2 0.7];
%=============================================================================
% A single sample with no style wears the hist default face colour.
Y = linspace(-3.2, 3.4, 400);
f = figure('Visible', 'off');
hist(Y);
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.FaceColor, HIST_FACE_COLOR, ...
  'hist must paint its bars in its own dark blue by default.');
close(f);
%=============================================================================
% The position axis is rounded outward onto round numbers.
f = figure('Visible', 'off');
hist(Y);
ax = gca();
assert_isequal(ax.XLim, [-4 4], ...
  'hist must round the position axis outward onto round numbers.');
close(f);
%=============================================================================
% The rounded limits always bracket the drawn bars.
f = figure('Visible', 'off');
hist(Y);
ax = gca();
drawn = ax.Children(1);
assert_istrue(ax.XLim(1) <= min(drawn.XData) && ax.XLim(2) >= max(drawn.XData), ...
  'The rounded limits must still contain every bar.');
close(f);
%=============================================================================
% The default colour also stands for a count of bins and for explicit centres.
f = figure('Visible', 'off');
hist(Y, 20);
ax = gca();
assert_isequal(ax.Children(1).FaceColor, HIST_FACE_COLOR);
close(f);
%=============================================================================
f = figure('Visible', 'off');
hist([1 2 2 3 3 3], [1 2 3]);
ax = gca();
assert_isequal(ax.Children(1).FaceColor, HIST_FACE_COLOR);
close(f);
%=============================================================================
% A style asked for by the caller still wins over the default colour, whether
% it comes as a line-spec or as a FaceColor property.
styles = {...
  'hist(Y, 10, ''r'')', ...
  'hist(Y, 10, ''FaceColor'', [1 0 0])'};
for k = 1:numel(styles)
  statement = styles{k};
  f = figure('Visible', 'off');
  eval([statement, ';']);
  ax = gca();
  drawn = ax.Children(1);
  assert_isequal(drawn.FaceColor, [1 0 0], ...
    [statement, ' must keep the colour the caller asked for.']);
  close(f);
end
%=============================================================================
% An edge colour on its own leaves the default face colour in place.
f = figure('Visible', 'off');
hist(Y, 10, 'EdgeColor', [0 1 0]);
ax = gca();
drawn = ax.Children(1);
assert_isequal(drawn.FaceColor, HIST_FACE_COLOR, ...
  'An EdgeColor on its own must not disturb the default face colour.');
assert_isequal(drawn.EdgeColor, [0 1 0]);
close(f);
%=============================================================================
% A matrix sample keeps the ColorOrder colours, one per column: the hist
% default is a single-sample affair only.
f = figure('Visible', 'off');
hist(randn(200, 3));
ax = gca();
drawn = ax.Children;
assert_istrue(numel(drawn) == 3, 'A three-column sample must draw three series.');
for k = 1:numel(drawn)
  assert_isfalse(isequal(drawn(k).FaceColor, HIST_FACE_COLOR), ...
    'A matrix sample must not force the single-sample hist colour.');
end
close(f);
%=============================================================================
