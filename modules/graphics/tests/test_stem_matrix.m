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
Y = [1 3; 2 4; 3 5];
h = stem(Y);
asserts.isequal(size(h), [1 2]);
asserts.istrue(isgraphics(h(1), 'stem'));
asserts.istrue(isgraphics(h(2), 'stem'));
asserts.isequal(h(1).XData, [1 2 3]);
asserts.isequal(h(1).YData, [1 2 3]);
asserts.isequal(h(1).XDataMode, 'auto');
asserts.isequal(h(1).YDataMode, 'manual');
asserts.isequal(h(2).XData, [1 2 3]);
asserts.isequal(h(2).YData, [3 4 5]);
asserts.isequal(h(2).XDataMode, 'auto');
asserts.isequal(h(2).YDataMode, 'manual');
asserts.isapprox(gca().XLim, [0.4666666666666667 3.533333333333333], 1e-12);
close all;
%=============================================================================
