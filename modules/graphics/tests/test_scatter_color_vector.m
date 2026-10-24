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
x = linspace(0, 3 * pi, 200);
y = cos(x) + rand(1, 200);
c = linspace(1, 10, length(x));
s = scatter(x, y, [], c);
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, c);
%=============================================================================
close all
%=============================================================================
