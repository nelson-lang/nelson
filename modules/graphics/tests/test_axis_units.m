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
f1 = figure('Position', [100 100 500 400]);
ax = gca();
positionNormalized = ax.Position;
outerPositionNormalized = ax.OuterPosition;
ax.Units = 'pixels';
positionPixels = ax.Position;
outerPositionPixels = ax.OuterPosition;
asserts.isfalse(isequal(positionNormalized, positionPixels));
%=============================================================================
f2 = figure('Position', [100 100 500 400]);
ax = gca();
asserts.isapprox(ax.Position, positionNormalized, 1e-4);
asserts.isequal(ax.OuterPosition, outerPositionNormalized);
asserts.isequal(ax.Units, 'normalized');
ax.Units = 'pixels';
asserts.isapprox(ax.Position, positionPixels, 1e-4);
asserts.isequal(ax.OuterPosition, outerPositionPixels);
asserts.isequal(ax.Units, 'pixels');
ax.Units = 'normalized';
asserts.isapprox(ax.Position, positionNormalized, 1e-4);
asserts.isequal(ax.OuterPosition, outerPositionNormalized);
asserts.isequal(ax.PositionMode, 'auto');
asserts.isequal(ax.Units, 'normalized');
%=============================================================================
