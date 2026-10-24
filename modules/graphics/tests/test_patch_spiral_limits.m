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
f = figure('Visible', 'off', 'Color', 'k');
ax = axes('Parent', f);
axes(ax);
ax.Color = 'k';
f = 0.1;
t = 0:f ^ 2:2 * pi;
r = pi / 4;
p = r * t + r;
patch([cos(p), 0], [sin(p), 0], 'y');
c = eye(3);
for a = 2:2:6
  patch([t / 4 + a, a + r * (1 + cos(t / 2)), a], ...
    [-f * cos(3 * (a + t)) - r, r * sin(t / 2), -1], c(a / 2, :));
  patch(a + f * cos(t)' + r ./ [1, 0.65], f * (2 + sin(t)') .* [1, 1], ...
    'k', 'EdgeColor', 'w', 'LineWidth', pi);
end
axis equal
axis off
tol = 1e-12;
asserts.istrue(ax.XLim(1) <= -1 + tol);
asserts.istrue(ax.XLim(2) >= 7.5 - tol);
asserts.istrue(ax.YLim(1) <= -1 + tol);
asserts.istrue(ax.YLim(2) >= 1 - tol);
%=============================================================================
