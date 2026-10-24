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
[X, Y, Z] = peaks(30);
[M, h] = contourf(ax, X, Y, Z, [0 0], '--r', 'FaceAlpha', 0.5, 'ShowText', 'on', 'LabelFormat', '%.1f');
asserts.isequal(h.Parent, ax);
asserts.isequal(h.LineStyle, '--');
asserts.isapprox(h.FaceAlpha, 0.5, 1e-12);
asserts.isequal(char(h.ShowText), 'on');
asserts.isequal(h.LabelFormat, '%.1f');
asserts.isequal(numel(h.LevelList), 1);
asserts.isequal(h.LevelList, 0);
asserts.isequal(M, h.ContourMatrix);
drawnow();
%=============================================================================
