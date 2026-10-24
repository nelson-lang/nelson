%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
fig = figure();
ax = axes('Parent', fig);
[X, Y, Z] = peaks(40);
[Cmat, h] = contour(ax, X, Y, Z);
levels = h.LevelList;
%=============================================================================
fig = figure();
ax = axes('Parent', fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t7 = clabel(Cmat, h, 'LabelSpacing', 100);
%=============================================================================
