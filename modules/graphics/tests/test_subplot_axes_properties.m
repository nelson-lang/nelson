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
ax1 = subplot(2, 1, 1);
Z = peaks();
plot(ax1, Z(1:20, :))
ax2 = subplot(2, 1, 2);
plot(ax2, Z);
ax1.FontSize = 15;
ax2.LineWidth = 2;
asserts.istrue(ax2.Position(2) >= 0.1);
asserts.isequal(char(ax1.Box), 'on');
asserts.isequal(char(ax2.Box), 'on');
close all;
%=============================================================================
