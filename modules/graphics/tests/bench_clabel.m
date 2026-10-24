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
tic();
fig = figure('Visible', 'off');
ax = axes('Parent', fig);
[X, Y] = meshgrid(-3:0.15:3, -3:0.15:3);
Z = peaks(X, Y);
[C, h] = contour(ax, X, Y, Z);
t9 = clabel(C, h, 'FontWeight', 'bold', 'LabelSpacing', 150);
for k = 1:4
  set(t9, 'Visible', 'off');
  drawnow();
  set(t9, 'Visible', 'on');
  drawnow();
end
toc()
close(fig);
%=============================================================================
