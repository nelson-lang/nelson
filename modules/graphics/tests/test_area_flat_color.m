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
% area: each series has a SeriesIndex (1, 2, ...); FaceColor and EdgeColor
% accept 'flat' (color from the colormap) and read it back.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
a = area(ax, [1 2; 3 4]);
assert_isequal(a(1).SeriesIndex, 1);
assert_isequal(a(2).SeriesIndex, 2);
a(1).FaceColor = 'flat';
assert_isequal(a(1).FaceColor, 'flat');
a(1).EdgeColor = 'flat';
assert_isequal(a(1).EdgeColor, 'flat');
drawnow();
a(2).FaceColor = [1 0 0];
assert_isequal(a(2).FaceColor, [1 0 0]);
delete(f);
%=============================================================================
