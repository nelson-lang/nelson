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
plot(1:10);
zlim([0, 11]);
ax = gca();
R = ax.ZLim;
REF = [0, 11];
asserts.isapprox(R, REF);
asserts.isequal(zlim('mode'), 'manual');
asserts.isequal(zlim(ax, 'mode'), 'manual');
%=============================================================================
f = figure();
plot3([1, 3], [4, 5], [10, 14]);
zlim('tight');
ax = gca();
asserts.isequal(zlim('method'), 'tight');
asserts.isequal(zlim(ax, 'method'), 'tight');
asserts.isequal(ax.ZLimMode, 'auto');
drawnow();
asserts.isapprox(ax.ZLim, [10, 14], 1e-12);
%=============================================================================
zlim('padded');
asserts.isequal(zlim('method'), 'padded');
asserts.isequal(ax.ZLimMode, 'auto');
drawnow();
asserts.istrue(ax.ZLim(1) <= 10 - 0.06 * 4);
asserts.istrue(ax.ZLim(1) >= 10 - 0.12 * 4);
asserts.istrue(ax.ZLim(2) >= 14 + 0.06 * 4);
asserts.istrue(ax.ZLim(2) <= 14 + 0.12 * 4);
%=============================================================================
zlim('tickaligned');
asserts.isequal(zlim('method'), 'tickaligned');
asserts.isequal(ax.ZLimMode, 'auto');
drawnow();
asserts.istrue(ax.ZLim(1) <= 10);
asserts.istrue(ax.ZLim(2) >= 14);
%=============================================================================
zlim([0, 20]);
asserts.isequal(ax.ZLimMode, 'manual');
zlim('tight');
asserts.isequal(ax.ZLimMode, 'auto');
asserts.isequal(ax.ZLimitMethod, 'tight');
%=============================================================================
