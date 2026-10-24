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
theta = linspace(0, 2 * pi, 150);
x = sin(theta) + 0.75 * rand(1, 150);
y = cos(theta) + 0.75 * rand(1, 150);
sz = 140;
s = scatter(x, y, sz, 'h');
asserts.isequal(s.Marker, 'hexagram');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.XData, x);
asserts.isequal(s.YData, y);
asserts.isequal(s.SizeData, 140);
asserts.isequal(s.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
close all
%=============================================================================
