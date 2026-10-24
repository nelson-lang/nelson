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
figure();
s = scatter(1, 1, 'x', 'r', 'LineWidth', 1.5);
asserts.isequal(s.Marker, 'x');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 1.5000);
asserts.isequal(s.XData, 1);
asserts.isequal(s.YData, 1);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [1 0 0]);
%=============================================================================
close all
%=============================================================================
