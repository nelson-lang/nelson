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
ylim([0, 11]);
ax = gca();
R = ax.YLim;
REF = [0, 11];
asserts.isapprox(R, REF);
asserts.isequal(ylim('mode'), 'manual');
asserts.isequal(ylim(ax, 'mode'), 'manual');
%=============================================================================
f = figure();
plot([1, 3], [4, 5]);
ylim('tight');
ax = gca();
asserts.isequal(ylim('method'), 'tight');
asserts.isequal(ylim(ax, 'method'), 'tight');
asserts.isequal(ax.YLimMode, 'auto');
drawnow();
asserts.isapprox(ax.YLim, [4, 5], 1e-12);
%=============================================================================
ylim('padded');
asserts.isequal(ylim('method'), 'padded');
asserts.isequal(ax.YLimMode, 'auto');
drawnow();
asserts.istrue(ax.YLim(1) <= 4 - 0.06);
asserts.istrue(ax.YLim(1) >= 4 - 0.20);
asserts.istrue(ax.YLim(2) >= 5 + 0.06);
asserts.istrue(ax.YLim(2) <= 5 + 0.20);
%=============================================================================
ylim('tickaligned');
asserts.isequal(ylim('method'), 'tickaligned');
asserts.isequal(ax.YLimMode, 'auto');
drawnow();
asserts.istrue(ax.YLim(1) <= 4 + 1e-12);
asserts.istrue(ax.YLim(2) >= 5 - 1e-12);
%=============================================================================
ylim([0, 6]);
asserts.isequal(ax.YLimMode, 'manual');
ylim('tight');
asserts.isequal(ax.YLimMode, 'auto');
asserts.isequal(ax.YLimitMethod, 'tight');
%=============================================================================
