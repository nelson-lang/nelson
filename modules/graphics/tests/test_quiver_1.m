%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
f.DrawLater = 'on';
[X, Y] = meshgrid(0:pi/8:pi, -pi:pi/8:pi);
U1 = sin(X);
V1 = cos(Y);
U2 = sin(Y);
V2 = cos(X); 
ax1 = subplot(1, 2, 1);
axis equal
title(ax1, 'Left Plot')
quiver(ax1, X, Y, U1, V1)
ax2 = subplot(1, 2, 2);
quiver(X,Y,U2,V2)
axis equal
title(ax2, 'Right Plot')
f.DrawLater = 'off';
%=============================================================================
