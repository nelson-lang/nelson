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
f = figure;
x = 1:5;
s1 = scatter(x, [6 3 9 10 7], 100, "filled");
asserts.isequal(s1.Marker, 'o');
asserts.isequal(s1.MarkerEdgeColor, 'none');
asserts.isequal(s1.MarkerFaceColor, 'flat');
asserts.isequal(s1.LineWidth, 0.5000);
asserts.isequal(s1.XData, [1 2 3 4 5]);
asserts.isequal(s1.YData, [6 3 9 10 7]);
asserts.isequal(s1.SizeData, 100);
asserts.isequal(s1.CData, [0.0660 0.4430 0.7450]);
hold on
s2 = scatter(x, [16 13 19 20 17], 100, "filled");
asserts.isequal(s2.Marker, 'o');
asserts.isequal(s2.MarkerEdgeColor, 'none');
asserts.isequal(s2.MarkerFaceColor, 'flat');
asserts.isequal(s2.LineWidth, 0.5000);
asserts.isequal(s2.XData, [1 2 3 4 5]);
asserts.isequal(s2.YData, [16 13 19 20 17]);
asserts.isequal(s2.SizeData, 100);
asserts.isequal(s2.CData, [0.8660 0.3290 0]);
s3 = scatter(x, [26 23 29 33 27], 100, "filled");
asserts.isequal(s3.Marker, 'o');
asserts.isequal(s3.MarkerEdgeColor, 'none');
asserts.isequal(s3.MarkerFaceColor, 'flat');
asserts.isequal(s3.LineWidth, 0.5000);
asserts.isequal(s3.XData, [1 2 3 4 5]);
asserts.isequal(s3.YData, [26 23 29 33 27]);
asserts.isequal(s3.SizeData, 100);
asserts.isequal(s3.CData, [0.9290 0.6940 0.1250]);
hold off
%=============================================================================
close all
%=============================================================================
