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
figure();
Z = peaks(25);
[M, h] = contourf(Z);
assert_isequal(class(h), 'graphics_object');
assert_isequal(h.Type, 'contour');
assert_isequal(h.FaceColor, 'flat');
assert_isequal(h.EdgeColor, [0 0 0]);
assert_isequal(M, h.ContourMatrix);
%=============================================================================
figure();
[X, Y, Z] = peaks(30);
[M, h] = contourf(X, Y, Z, 6);
assert_isequal(M, h.ContourMatrix);
assert_isequal(numel(h.LevelList), 6);
%=============================================================================
fig = figure();
ax = axes('Parent', fig);
[M, h] = contourf(ax, X, Y, Z, [0 0], '--r', 'FaceAlpha', 0.5, 'ShowText', 'on', 'LabelFormat', '%.1f');
assert_isequal(h.Parent, ax);
assert_isequal(h.LineStyle, '--');
assert_isapprox(h.FaceAlpha, 0.5, 1e-12);
assert_isequal(h.ShowText, 'on');
assert_isequal(h.LabelFormat, '%.1f');
assert_isequal(numel(h.LevelList), 1);
assert_isequal(h.LevelList, 0);
assert_isequal(M, h.ContourMatrix);
drawnow();
%=============================================================================
figure();
Z = peaks(25);
Z(:, 13) = NaN;
[M, h] = contourf(Z, 4, 'LineWidth', 2, 'LabelColor', 'black', 'ZLocation', 'zmin');
assert_isequal(M, h.ContourMatrix);
assert_isequal(h.LineWidth, 2);
assert_isequal(h.LabelColor, [0 0 0]);
assert_isequal(h.ZLocation, 'zmin');
%=============================================================================
figure();
[M, h] = contourf(peaks(20), 5, 'ShowText', 'on', 'TextList', [0 2], 'LabelFormat', @(x) sprintf('L%g', x));
assert_isequal(M, h.ContourMatrix);
assert_isequal(h.ShowText, 'on');
assert_isequal(h.TextList, [0 2]);
assert_isequal(class(h.LabelFormat), 'function_handle');
drawnow();
%=============================================================================
