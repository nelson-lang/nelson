%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
reset(groot());
f = figure('Visible', 'off');
N = 20;
x = linspace(-1, 1, N);
y = linspace(-1.4, .6, N);
M = reshape(1:(N * N), N, N);
im = imagesc(x, y, M);
asserts.isequal(length(im.XData), N);
asserts.isequal(length(im.YData), N);
colormap([1 1 1; 1 0 0]);
ax = gca();
asserts.isequal(ax.YDir, 'reverse');
ax.YDir = 'normal';
asserts.isequal(ax.YDir, 'normal');
close(f);
close all;
%=============================================================================
