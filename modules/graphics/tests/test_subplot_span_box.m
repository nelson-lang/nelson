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
subplot(2, 2, 1);
x = linspace(-3.8, 3.8);
y_cos = cos(x);
plot(x, y_cos);
title('Subplot 1: Cosine')
subplot(2, 2, 2);
y_poly = 1 - x .^ 2 ./ 2 + x .^ 4 ./ 24;
plot(x, y_poly, 'g');
title('Subplot 2: Polynomial')
subplot(2, 2, [3, 4]);
plot(x, y_cos, 'b', x, y_poly, 'g');
title('Subplot 3 and 4: Both')
ax = gca();
asserts.isapprox(ax.Position, [0.13 0.11 0.775 0.341162790697674], 1e-12);
%=============================================================================
f = figure();
ax1 = subplot(2, 2, 1);
asserts.isequal(char(ax1.Box), 'off');
plot(1:3);
asserts.isequal(char(ax1.Box), 'on');
ax2 = subplot(2, 2, 2);
asserts.isequal(char(ax2.Box), 'off');
plot(1:3);
asserts.isequal(char(ax2.Box), 'on');
close(f);
%=============================================================================
f = figure();
ax = axes();
ax.Box = 'off';
hold(ax, 'on');
plot(ax, 1:3);
asserts.isequal(char(ax.Box), 'off');
close(f);
close all;
%=============================================================================
