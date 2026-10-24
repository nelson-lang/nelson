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
f = figure();
ax1 = subplot(2, 2, 1)
x = linspace(0, 10);
y1 = sin(x);
plot(x, y1)
title('Subplot 1: sin(x)')
ax2 = subplot(2, 2, 2)
y2 = sin(2 * x);
plot(x, y2)
title('Subplot 2: sin(2x)')
ax3 = subplot(2, 2, 3)
y3 = sin(4 * x);
plot(x, y3)
title('Subplot 3: sin(4x)')
ax4 = subplot(2, 2, 4)
y4 = sin(8 * x);
plot(x, y4)
title('Subplot 4: sin(8x)')
asserts.isapprox(ax1.Position, [0.13 0.583837209302326 0.334659090909091 0.341162790697674], 1e-12);
asserts.isapprox(ax2.Position, [0.570340909090909 0.583837209302326 0.334659090909091 0.341162790697674], 1e-12);
asserts.isapprox(ax3.Position, [0.13 0.11 0.334659090909091 0.341162790697674], 1e-12);
asserts.isapprox(ax4.Position, [0.570340909090909 0.11 0.334659090909091 0.341162790697674], 1e-12);
close all;
%=============================================================================
