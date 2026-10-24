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
% Filled contour of a 100x100 grid with 20 levels. The contour is drawn at
% native grid resolution (each cell crossing is a vertex), so this stays
% interactive; it guards against re-introducing a whole-grid upsampling pass
% that made the same plot render several seconds slower.
fig = figure('Visible', 'off');
x = linspace(-2 * pi, 2 * pi, 100);
y = linspace(-2 * pi, 2 * pi, 100);
[X, Y] = meshgrid(x, y);
Z = sin(X) .* cos(Y);
contourf(X, Y, Z, 20);
drawnow();
tic();
contourf(X, Y, Z, 20);
drawnow();
toc()
close(fig);
%=============================================================================
