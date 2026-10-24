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
s = scatter(x, y, sz, 'd');
asserts.isequal(s.Marker, 'diamond');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.SizeData, 140)
%=============================================================================
close all
%=============================================================================
