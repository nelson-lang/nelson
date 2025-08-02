%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/823
% <-- Short Description -->
% default LineStyle for a line is wrong with marker.
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
x = linspace(-pi*7/8,pi*7/8,15);
y = cos(x);
h = plot(x, y, 'ro');
assert_isequal(h.LineStyle, 'none');
%=============================================================================
