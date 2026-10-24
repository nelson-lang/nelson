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
figure;
y = rand(1, 5);
s = scatter(1:5, y, 100, 'b', 'LineWidth', 2);
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 2);
asserts.isequal(s.XData, [1 2 3 4 5]);
asserts.isequal(s.YData, y);
asserts.isequal(s.SizeData, 100);
asserts.isequal(s.CData, [0 0 1]);
%=============================================================================
close all
%=============================================================================
