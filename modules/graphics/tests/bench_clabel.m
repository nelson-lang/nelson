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
fig = figure();
ax = axes('Parent', fig);
[X, Y] = meshgrid(-3:0.1:3, -3:0.1:3);
Z = peaks(X, Y);

% Draw contour
[C, h] = contour(ax, X, Y, Z);

% Add contour labels on selected levels
t9 = clabel(C, h, 'FontWeight', 'bold', 'LabelSpacing', 150);

% Make labels blink
for k = 1:20  % number of blinks
    for i = 1:numel(t9)
        if strcmp(t9(i).Visible, 'on')
            t9(i).Visible = 'off';
        else
            t9(i).Visible = 'on';
        end
    end
    pause(0.5);
end
%=============================================================================
