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
% A 4-element RGBA color is accepted and stored as given (alpha is the line
% opacity, ColorAlpha).
h = line('Parent', ax, 'XData', 1:3, 'YData', 1:3, 'Color', [0.95 0.4 0.25 0.5]);
asserts.isequal(get(h, 'Color'), [0.95 0.4 0.25 0.5]);
%=============================================================================
% A 3-element RGB color still works and stays a 3-vector.
set(h, 'Color', [0.1 0.2 0.3]);
asserts.isequal(get(h, 'Color'), [0.1 0.2 0.3]);
%=============================================================================
% Switching between RGBA and RGB keeps the exact size given.
set(h, 'Color', [0 0.4470 0.7410 0.25]);
asserts.isequal(get(h, 'Color'), [0 0.4470 0.7410 0.25]);
set(h, 'Color', [1 0 0]);
asserts.isequal(get(h, 'Color'), [1 0 0]);
%=============================================================================
% Named colors still resolve to a 3-vector.
set(h, 'Color', 'red');
asserts.isequal(get(h, 'Color'), [1 0 0]);
%=============================================================================
% RGBA given at construction time on plot3 survives.
p = plot3(ax, 1:3, 1:3, 1:3, 'Color', [0.2 0.6 0.9 0.75], 'LineWidth', 0.75);
asserts.isequal(get(p, 'Color'), [0.2 0.6 0.9 0.75]);
%=============================================================================
% RGBA given at construction time on plot survives.
q = plot(ax, 1:3, 1:3, 'Color', [0.3 0.7 0.1 0.4]);
asserts.isequal(get(q, 'Color'), [0.3 0.7 0.1 0.4]);
%=============================================================================
% An out-of-range alpha is rejected.
asserts.checkerror('set(h, ''Color'', [0.1 0.2 0.3 1.5]);', graphicsPropertyErrorMessage('Color', 'Nelson:color:invalidColorSpec'), ...
  'Nelson:graphics:propertyError');
%=============================================================================
% A 5-element color is still rejected.
asserts.checkerror('set(h, ''Color'', [0.1 0.2 0.3 0.4 0.5]);', graphicsPropertyErrorMessage('Color', 'Nelson:color:invalidColorSpec'), ...
  'Nelson:graphics:propertyError');
%=============================================================================
% A 2-element color is still rejected.
asserts.checkerror('set(h, ''Color'', [0.1 0.2]);', graphicsPropertyErrorMessage('Color', 'Nelson:color:invalidColorSpec'), ...
  'Nelson:graphics:propertyError');
%=============================================================================
close(f);
close all;
%=============================================================================
