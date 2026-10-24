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
sz = linspace(1, 100, 200);
s = scatter(x, y, sz);
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.XData, x);
asserts.isequal(s.YData, y);
asserts.isequal(s.SizeData, sz);
asserts.isequal(s.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
close all
%=============================================================================
