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
f = figure();
ax = gca();
[X, Y] = meshgrid(1:0.5:10, 1:20);
Z = sin(X) + cos(Y);
h = surface(ax, X, Y, Z);
asserts.isapprox(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
ax2 = axes('Parent', f);
axes(ax1);
h = surface(peaks(4));
asserts.isequal(h.Parent, ax1);
axes(ax2);
h = surface(peaks(4));
asserts.isequal(h.Parent, ax2);
close(f);
close all;
%=============================================================================
