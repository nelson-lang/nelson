%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
X = linspace(0, 2 * pi, 50)';
Y = cos(X);
h = stem(X, Y)
asserts.isequal(h.XDataMode, 'manual');
asserts.isequal(h.YDataMode, 'manual');
%=============================================================================
f = figure();
X = linspace(0, 2 * pi, 50)';
Y = cos(X);
h = stem(X, Y, 'filled');
asserts.isequal(h.MarkerFaceColor, 'auto');
%=============================================================================
f = figure();
x1 = linspace(0, 2 * pi, 50)';
x2 = linspace(pi, 3 * pi, 50)';
X = [x1, x2];
Y = [cos(x1), 0.5 * sin(x2)];
h = stem(X, Y, 'filled');
asserts.isequal(size(h), [1 2]);
asserts.isequal(h(1).MarkerFaceColor, 'auto');
asserts.isequal(h(2).MarkerFaceColor, 'auto');
close all;
%=============================================================================
