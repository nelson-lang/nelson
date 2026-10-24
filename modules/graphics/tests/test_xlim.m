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
plot3([0, 1], [0, 1], [0, 1]);
xlim([0, 1.1]);
ax = gca();
R = ax.XLim;
REF = [0, 1.1];
asserts.isapprox(R, REF);
asserts.isequal(xlim('mode'), 'manual');
asserts.isequal(xlim(ax, 'mode'), 'manual');
%=============================================================================
f = figure();
plot([1, 3], [4, 5]);
xlim('tight');
ax = gca();
asserts.isequal(xlim('method'), 'tight');
asserts.isequal(xlim(ax, 'method'), 'tight');
asserts.isequal(ax.XLimMode, 'auto');
drawnow();
asserts.isapprox(ax.XLim, [1, 3], 1e-12);
%=============================================================================
xlim('padded');
asserts.isequal(xlim('method'), 'padded');
asserts.isequal(ax.XLimMode, 'auto');
drawnow();
asserts.istrue(ax.XLim(1) <= 1 - 0.06 * 2);
asserts.istrue(ax.XLim(1) >= 1 - 0.20 * 2);
asserts.istrue(ax.XLim(2) >= 3 + 0.06 * 2);
asserts.istrue(ax.XLim(2) <= 3 + 0.20 * 2);
%=============================================================================
xlim('tickaligned');
asserts.isequal(xlim('method'), 'tickaligned');
asserts.isequal(ax.XLimMode, 'auto');
drawnow();
asserts.istrue(ax.XLim(1) <= 1);
asserts.istrue(ax.XLim(2) >= 3);
%=============================================================================
xlim([0, 5]);
asserts.isequal(ax.XLimMode, 'manual');
xlim('tight');
asserts.isequal(ax.XLimMode, 'auto');
asserts.isequal(ax.XLimitMethod, 'tight');
%=============================================================================
