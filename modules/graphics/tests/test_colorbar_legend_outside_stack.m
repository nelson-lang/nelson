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
% Outside legends and colorbars of one axes stack on their side in arrival order
% (8pt apart) and the axes shrinks for all of them.
%=============================================================================
tol = 1e-2;
%=============================================================================
% legend first, then the colorbar to its right
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
cb = colorbar(ax);
drawnow();
assert_isapprox(ax.Position, [0.078542 0.11 0.46823 0.815], tol);
assert_isapprox(lg.Position, [0.59167 0.86667 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.82183 0.10933 0.0468 0.816], tol);
assert_stacked(lg, cb);
% deleting the colorbar gives its room back
delete(cb);
drawnow();
assert_isapprox(ax.Position, [0.097608 0.11 0.5819 0.815], tol);
assert_isapprox(lg.Position, [0.72567 0.86667 0.2035 0.058667], tol);
close(f);
%=============================================================================
% colorbar first, then the legend to its right
[f, ax] = make_axes();
cb = colorbar(ax);
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
drawnow();
assert_isapprox(ax.Position, [0.078542 0.11 0.46823 0.815], tol);
assert_isapprox(lg.Position, [0.7318 0.86667 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.59167 0.10933 0.0468 0.816], tol);
assert_stacked(cb, lg);
% deleting the legend gives its room back
delete(lg);
drawnow();
assert_isapprox(ax.Position, [0.10846 0.11 0.64661 0.815], tol);
assert_isapprox(cb.Position, [0.80167 0.10933 0.053333 0.816], tol);
close(f);
%=============================================================================
% an 'eastoutside' legend is centered on the box, the colorbar still after it
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'eastoutside';
cb = colorbar(ax);
drawnow();
assert_isapprox(ax.Position, [0.078542 0.11 0.46823 0.815], tol);
assert_isapprox(lg.Position, [0.59167 0.488 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.82183 0.10933 0.0468 0.816], tol);
close(f);
%=============================================================================
% a legend moved inside leaves the stack; back outside it comes after the colorbar
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
cb = colorbar(ax);
drawnow();
lg.Location = 'northeast';
drawnow();
assert_isapprox(ax.Position, [0.10846 0.11 0.64661 0.815], tol);
assert_isapprox(lg.Position, [0.52583 0.83111 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.80167 0.10933 0.053333 0.816], tol);
lg.Location = 'northeastoutside';
drawnow();
assert_isapprox(ax.Position, [0.078542 0.11 0.46823 0.815], tol);
assert_isapprox(lg.Position, [0.7318 0.86667 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.59167 0.10933 0.0468 0.816], tol);
close(f);
%=============================================================================
% different sides do not stack
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
cb = colorbar(ax, 'westoutside');
drawnow();
assert_isapprox(ax.Position, [0.26987 0.11 0.42845 0.815], tol);
assert_isapprox(lg.Position, [0.74367 0.86667 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.1492 0.10933 0.0428 0.816], tol);
close(f);
%=============================================================================
% north side: legend above the box, colorbar above the legend
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'northoutside';
cb = colorbar(ax, 'northoutside');
drawnow();
assert_isapprox(ax.Position, [0.13 0.075338 0.775 0.55818], tol);
assert_isapprox(lg.Position, [0.41625 0.69556 0.2035 0.058667], tol);
assert_isapprox(cb.Position, [0.13 0.78978 0.776 0.056], tol);
assert_istrue(lg.Position(2) + lg.Position(4) < cb.Position(2));
close(f);
%=============================================================================
% pixel units read the same boxes
[f, ax] = make_axes();
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
cb = colorbar(ax);
drawnow();
cbNormalized = cb.Position;
lgNormalized = lg.Position;
cb.Units = 'pixels';
lg.Units = 'pixels';
drawnow();
assert_isapprox(cb.Position, cbNormalized .* [400 300 400 300] + [1 1 0 0], 1e-6);
assert_isapprox(lg.Position, lgNormalized .* [400 300 400 300] + [1 1 0 0], 1e-6);
% a resize keeps the decorations in pixels and the stack order
f.Position = [100 100 600 450];
drawnow();
assert_isapprox(lg.Position(3:4), lgNormalized(3:4) .* [400 300], 1e-6);
cb.Units = 'normalized';
lg.Units = 'normalized';
assert_stacked(lg, cb);
assert_istrue(ax.Position(1) + ax.Position(3) < lg.Position(1));
close(f);
%=============================================================================
% same stack inside a uipanel, in the panel units
f = figure('Visible', 'off', 'Position', [100 100 700 500]);
p = uipanel(f, 'Units', 'pixels', 'Position', [50 50 400 300]);
ax = axes('Parent', p);
plot(ax, 1:10);
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
cb = colorbar(ax);
drawnow();
assert_istrue(isequal(cb.Parent, p));
assert_isapprox(ax.Position, [0.078231 0.11 0.46638 0.815], tol);
assert_isapprox(lg.Position, [0.59121 0.86559 0.20473 0.05914], tol);
assert_isapprox(cb.Position, [0.82277 0.11022 0.04668 0.81452], tol);
assert_stacked(lg, cb);
close(f);
%=============================================================================
% and inside a uitab
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
tg = uitabgroup(f);
t = uitab(tg);
ax = axes('Parent', t);
plot(ax, 1:10);
cb = colorbar(ax);
lg = legend(ax, 'data1');
lg.Location = 'northeastoutside';
drawnow();
assert_istrue(isequal(cb.Parent, t));
assert_stacked(cb, lg);
assert_istrue(ax.Position(1) + ax.Position(3) < cb.Position(1));
assert_istrue(lg.Position(1) + lg.Position(3) <= 1);
close(f);
%=============================================================================
function [f, ax] = make_axes()
  f = figure('Visible', 'off', 'Position', [100 100 400 300]);
  ax = axes(f);
  plot(ax, 1:10);
end
%=============================================================================
function assert_stacked(first, second)
  % second starts right of first, 8pt (about 10.7 px) apart at least
  p1 = first.Position;
  p2 = second.Position;
  assert_istrue(p1(1) + p1(3) < p2(1));
  assert_istrue(p2(1) + p2(3) <= 1);
end
%=============================================================================
