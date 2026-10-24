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
x = rand(100, 1);
y = rand(100, 1);
MarkerColor = [0.5, 0.5, 0.5];
s = scatter(x, y, 40, MarkerColor, 'filled');
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'none');
asserts.isequal(s.MarkerFaceColor, 'flat');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.SizeData, 40);
asserts.isequal(s.CData, [0.5000 0.5000 0.5000]);
%=============================================================================
close all
%=============================================================================
