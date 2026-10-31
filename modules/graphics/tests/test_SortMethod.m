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
% SortMethod 'depth' paints 3D children back-to-front by projected depth;
% 'childorder' keeps the Children order. Both backends are depth-ordered
% (web list and GL z-buffer), so this checks the property round-trips and the
% 'depth' draw order renders without error.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
view(ax, 3);
plot3(ax, [0 0.1], [0 0], [0 0]);
hold(ax, 'on');
plot3(ax, [0.9 1], [1 1], [1 1]);
%=============================================================================
% default SortMethod is 'childorder'
assert_isequal(char(ax.SortMethod), 'childorder');
%=============================================================================
ax.SortMethod = 'depth';
assert_isequal(char(ax.SortMethod), 'depth');
drawnow();
dl = char(__web_display_list__(f));
assert_istrue(~isempty(strfind(dl, 'polyline')));
%=============================================================================
ax.SortMethod = 'childorder';
assert_isequal(char(ax.SortMethod), 'childorder');
drawnow();
%=============================================================================
close(f);
%=============================================================================
