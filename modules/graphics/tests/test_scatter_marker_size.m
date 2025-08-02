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
f = figure();
% Create figure
hold on;
% Settings
nPoints = 10; % Number of points per marker type
markers = {'o', '+', '*', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};
sizesMin = 20; % Minimum size
sizesMax = 100; % Maximum size
% X positions
x = linspace(1, 10, nPoints);
% Fixed color
fixedColor = [0 0 0]; % black
% Plot each marker type
for m = 1:numel(markers)
    y = m * ones(size(x)); % Constant Y for each marker type
    sizes = linspace(sizesMin, sizesMax, nPoints); % Increasing sizes
    % Scatter points
    scatter(x, y, sizes, ...
        'Marker', markers{m}, ...
        'MarkerEdgeColor', fixedColor, ...
        'MarkerFaceColor', 'none', ...
        'LineWidth', 1.5);
end
title('Scatter Only - One Line per Marker Type with Increasing Size');
xlabel('X Axis');
ylabel('Marker Type Line');
ylim([0 numel(markers)+1]);
hold off;
