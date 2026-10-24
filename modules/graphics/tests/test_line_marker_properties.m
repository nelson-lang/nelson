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
reset(groot());
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
h = line('Parent', ax, 'XData', 1:3, 'YData', 1:3);
asserts.isequal(get(h, 'Marker'), 'none');
asserts.isequal(get(h, 'MarkerSize'), 6);
asserts.isequal(get(h, 'MarkerFaceColor'), 'none');
asserts.isequal(get(h, 'MarkerEdgeColor'), 'auto');
%=============================================================================
symbols = {'o', '+', '*', '.', 'x', '^', 'v', '>', '<', 'none'};
for k = 1:numel(symbols)
  set(h, 'Marker', symbols{k});
  asserts.isequal(get(h, 'Marker'), symbols{k});
end
% the one-letter aliases canonicalize to their full name
aliases = {'s', 'square'; 'd', 'diamond'; 'p', 'pentagram'; 'h', 'hexagram'};
for k = 1:size(aliases, 1)
  set(h, 'Marker', aliases{k, 1});
  asserts.isequal(get(h, 'Marker'), aliases{k, 2});
  set(h, 'Marker', aliases{k, 2});
  asserts.isequal(get(h, 'Marker'), aliases{k, 2});
end
%=============================================================================
set(h, 'MarkerSize', 12.5);
asserts.isequal(get(h, 'MarkerSize'), 12.5);
set(h, 'MarkerFaceColor', [0.25 0.5 0.75]);
asserts.isequal(get(h, 'MarkerFaceColor'), [0.25 0.5 0.75]);
set(h, 'MarkerEdgeColor', [1 0 0]);
asserts.isequal(get(h, 'MarkerEdgeColor'), [1 0 0]);
set(h, 'MarkerFaceColor', 'red');
asserts.isequal(get(h, 'MarkerFaceColor'), [1 0 0]);
set(h, 'MarkerEdgeColor', 'none');
asserts.isequal(get(h, 'MarkerEdgeColor'), 'none');
%=============================================================================
% Marker properties given at construction time must survive.
c = line('Parent', ax, 'XData', 1:3, 'YData', 1:3, 'LineStyle', 'none', ...
         'Marker', '^', 'MarkerSize', 9, 'MarkerFaceColor', [0 1 0], ...
         'MarkerEdgeColor', [0 0 1]);
asserts.isequal(get(c, 'Marker'), '^');
asserts.isequal(get(c, 'MarkerSize'), 9);
asserts.isequal(get(c, 'MarkerFaceColor'), [0 1 0]);
asserts.isequal(get(c, 'MarkerEdgeColor'), [0 0 1]);
asserts.isequal(get(c, 'LineStyle'), 'none');
%=============================================================================
% A marker-only linespec sets Marker and drops the line.
p = plot(ax, 1:3, 1:3, '^', 'MarkerSize', 8, 'MarkerFaceColor', [1 1 0]);
asserts.isequal(get(p, 'Marker'), '^');
asserts.isequal(get(p, 'LineStyle'), 'none');
asserts.isequal(get(p, 'MarkerSize'), 8);
asserts.isequal(get(p, 'MarkerFaceColor'), [1 1 0]);
%=============================================================================
q = plot(ax, 1:3, 1:3, 'Color', [0 1 1], 'Marker', 'v', 'MarkerSize', 8);
asserts.isequal(get(q, 'Marker'), 'v');
set(q, 'LineStyle', 'none');
asserts.isequal(get(q, 'LineStyle'), 'none');
asserts.isequal(get(q, 'Marker'), 'v');
%=============================================================================
asserts.checkerror('set(h, ''Marker'', ''Z'');', graphicsPropertyErrorMessage('Marker', 'Nelson:graphics:illegalSelection'), ...
  'Nelson:graphics:propertyError');
%=============================================================================
close(f);
close all;
%=============================================================================
