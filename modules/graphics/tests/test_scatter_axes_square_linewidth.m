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
close all
%=============================================================================
rng('default') % For reproducibility
f = figure;
ax = gca();
s = scatter(ax, linspace(0, 10, 10), rand(1, 10), 's', 'g', 'LineWidth', 2);
asserts.isequal(s.Marker, 'square');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 2);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [0 1 0]);
%=============================================================================
close all
%=============================================================================
