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
figure();
x = rand(1, 100);
y = rand(1, 100);
s = scatter(x, y, 75, "MarkerEdgeColor", "b", "MarkerFaceColor", [0 0.7 0.7]);
asserts.isequal(s.Marker, 'o')
asserts.isequal(s.MarkerEdgeColor, [0 0 1])
asserts.isequal(s.MarkerFaceColor, [0 0.7000 0.7000])
asserts.isequal(s.LineWidth, 0.5000)
asserts.isequal(s.XData, x)
asserts.isequal(s.YData, y)
asserts.isequal(s.SizeData, 75)
asserts.isequal(s.CData, [0.0660 0.4430 0.7450])
%=============================================================================
close all
%=============================================================================
