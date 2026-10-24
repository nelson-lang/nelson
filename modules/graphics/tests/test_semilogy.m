%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
f = figure();
x = logspace(-1, 2);
semilogy(x, x);
grid on
%=============================================================================
f = figure();
x = logspace(-1, 2, 15);
y = 13 + x;
semilogy(x, y, 'x', 'MarkerFaceColor', [0 0.447 0.741])
grid on
%=============================================================================
f = figure('Visible', 'off');
x = 1:100;
y1 = x .^ 2;
y2 = x .^ 3;
semilogy(x, y1, '--', x, y2);
legend('x^2', 'x^3', 'Location', 'northwest');
drawnow();
ax = gca();
asserts.isequal(ax.YScale, 'log');
asserts.isequal(char(ax.YMinorTick), 'on');
asserts.isequal(ax.YTick, [1 10 100 1000 10000 100000 1000000]);
asserts.isequal(ax.YTickLabel, {'10^{0}'; '10^{1}'; '10^{2}'; '10^{3}'; '10^{4}'; ...
  '10^{5}'; '10^{6}'});
close(f);
%=============================================================================
