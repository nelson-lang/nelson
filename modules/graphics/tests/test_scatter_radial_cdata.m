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
x = randn(100, 1);
y = randn(100, 1);
c = sqrt(x .^ 2 + y .^ 2);
s = scatter(x, y, [], c);
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isapprox(s.XData, x(:)', 1e-5);
asserts.isapprox(s.YData, y(:)', 1e-5);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, c);
%=============================================================================
close all
%=============================================================================
