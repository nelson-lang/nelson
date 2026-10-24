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
f = figure();
c = 1:4;
s = scatter(1:4, [2 5 3 7], [], c);
colormap(gca, 'winter')
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.XData, [1 2 3 4]);
asserts.isequal(s.YData, [2 5 3 7]);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [1 2 3 4]);
%=============================================================================
close all
%=============================================================================
