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
f  = figure();
colormap = [0.2 0.1 0.5;
    0.1 0.5 0.8;
    0.2 0.7 0.6;
    0.8 0.7 0.3;
    0.9 1 0];
rgbplot(colormap);
ax = gca();
assert_isequal(size(ax.Children), [3 1]);
assert_isequal(ax.Children(1).Type, 'line');
assert_isequal(ax.Children(2).Type, 'line');
assert_isequal(ax.Children(3).Type, 'line');
%=============================================================================
