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
figure;
X = [1 2 3; 4 5 6];
Y = [2 3 4; 5 6 7];
s = scatter(X, Y, 'x', 'm', 'LineWidth', 2);
asserts.isequal(size(s), [1 3]);
asserts.isequal(s(1).Marker, 'x');
asserts.isequal(s(1).MarkerEdgeColor, 'flat');
asserts.isequal(s(1).MarkerFaceColor, 'none');
asserts.isequal(s(1).LineWidth, 2);
asserts.isequal(s(1).XData, [1 4]);
asserts.isequal(s(1).YData, [2 5]);
asserts.isequal(s(1).SizeData, 36);
asserts.isequal(s(1).CData, [1 0 1]);
asserts.isequal(s(2).Marker, 'x');
asserts.isequal(s(2).MarkerEdgeColor, 'flat');
asserts.isequal(s(2).MarkerFaceColor, 'none');
asserts.isequal(s(2).LineWidth, 2);
asserts.isequal(s(2).XData, [2 5]);
asserts.isequal(s(2).YData, [3 6]);
asserts.isequal(s(2).SizeData, 36);
asserts.isequal(s(2).CData, [1 0 1]);
asserts.isequal(s(3).Marker, 'x');
asserts.isequal(s(3).MarkerEdgeColor, 'flat');
asserts.isequal(s(3).MarkerFaceColor, 'none');
asserts.isequal(s(3).LineWidth, 2);
asserts.isequal(s(3).XData, [3 6]);
asserts.isequal(s(3).YData, [4 7]);
asserts.isequal(s(3).SizeData, 36);
asserts.isequal(s(3).CData, [1 0 1]);
%=============================================================================
close all
%=============================================================================
