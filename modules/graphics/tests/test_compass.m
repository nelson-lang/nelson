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
% Pin the calibration geometry for 573x460 on a 1024x768 CI runner.
set(groot, 'DefaultFigurePosition', [423 180 1074 646]);
%=============================================================================
% compass draws one Line object per vector (arrows from the origin).
f = figure('Visible', 'off');
h = compass(rand(1, 8), rand(1, 8));
asserts.istrue(~isempty(h));
asserts.istrue(all(isgraphics(h, 'line')));
asserts.istrue(all(strcmp(get(h, 'Type'), 'line')));
asserts.isequal(size(h), [8 1]);
asserts.isequal(get(ancestor(h(1), 'axes'), 'Type'), 'axes');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compass([1 + 2i, 2 - 1i, -1 + 1i]);
ax = ancestor(h(1), 'axes');
asserts.isequal(ax.XTick, -2:2);
close(f);
%=============================================================================
% Each arrow is a shaft plus a two-segment arrowhead scaled by the vector.
f = figure('Visible', 'off');
h = compass([1 0], [0 1]);
asserts.isequal(size(h), [2 1]);
asserts.isapprox(h(1).XData, [0 1 0.8 1 0.8], 1e-12);
asserts.isapprox(h(1).YData, [0 0 0.08 0 -0.08], 1e-12);
asserts.isapprox(h(2).XData, [0 0 -0.08 0 0.08], 1e-12);
asserts.isapprox(h(2).YData, [0 1 0.8 1 0.8], 1e-12);
close(f);
%=============================================================================
% compass(Z) uses the real and imaginary parts as U and V.
f = figure('Visible', 'off');
h = compass(complex(1, 1));
asserts.isequal(size(h), [1 1]);
asserts.istrue(isgraphics(h, 'line'));
asserts.isapprox(h.XData, [0 1 0.72 1 0.88], 1e-12);
asserts.isapprox(h.YData, [0 1 0.88 1 0.72], 1e-12);
close(f);
%=============================================================================
% Every arrow shares the same default color (no color cycling).
f = figure('Visible', 'off');
h = compass([1 3 2], [2 1 -1]);
asserts.isequal(h(1).Color, h(2).Color);
asserts.isequal(h(1).Color, h(3).Color);
asserts.isequal(h(1).LineWidth, 0.5);
asserts.isequal(h(1).Marker, 'none');
close(f);
%=============================================================================
% An explicit parent axes receives the arrows.
f = figure('Visible', 'off');
ax = axes();
h = compass(ax, [1 2], [3 4]);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(2).Parent, ax);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compass([1 2], [3 4], '-r');
asserts.isequal(h(1).Color, [1 0 0]);
asserts.isequal(h(2).Color, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compass([1 2], [3 4], '--b', 'LineWidth', 2);
asserts.isequal(h(1).Color, [0 0 1]);
asserts.isequal(h(1).LineStyle, '--');
asserts.isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compass([1 2], [3 4], 'LineWidth', 2);
asserts.isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================
% A marker in the LineSpec is honored.
f = figure('Visible', 'off');
h = compass([1 2], [3 4], '-or');
asserts.isequal(h(1).Color, [1 0 0]);
asserts.isequal(h(1).Marker, 'o');
close(f);
%=============================================================================
% Matrix input produces one arrow per element (column-major).
f = figure('Visible', 'off');
h = compass([1 0; 0 1], [0 1; 1 0]);
asserts.isequal(size(h), [4 1]);
asserts.isapprox(h(1).XData, [0 1 0.8 1 0.8], 1e-12);
asserts.isapprox(h(2).XData, [0 0 -0.08 0 0.08], 1e-12);
close(f);
%=============================================================================
caught = false;
try
  compass([1 2], [3 4 5]);
catch
  caught = true;
end
asserts.istrue(caught);
caught = false;
try
  compass({}, 1);
catch
  caught = true;
end
asserts.istrue(caught);
%=============================================================================
