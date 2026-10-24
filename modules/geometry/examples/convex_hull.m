%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compute and display the convex hull of a point set.
points = [0, 0; 1, 0; 1.2, 0.7; 0.5, 1.3; -0.2, 0.8; 0.5, 0.5];
indices = convhull(points(:, 1), points(:, 2));
figure();
plot(points(:, 1), points(:, 2), 'o');
hold on;
plot(points(indices, 1), points(indices, 2), '-');
axis equal;
