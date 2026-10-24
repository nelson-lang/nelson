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
s = scatter(1, 1, 'o', 'g', 'filled', 'LineWidth', 2);
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'none');
asserts.isequal(s.MarkerFaceColor, 'flat');
asserts.isequal(s.LineWidth, 2);
asserts.isequal(s.XData, 1);
asserts.isequal(s.YData, 1);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [0 1 0]);
%=============================================================================
close all
%=============================================================================
