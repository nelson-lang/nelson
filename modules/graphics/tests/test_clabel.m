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
fig = figure();
ax = axes('Parent',fig);
[X,Y,Z] = peaks(40);
[Cmat, h] = contour(ax, X, Y, Z);
t1 = clabel(Cmat, h);
assert_isequal(size(t1), [1, 31])
assert_isequal(class(t1), 'graphics_object');
for i = 1:numel(t1)
  assert_isequal(t1(i).Type, 'text');
end
%=============================================================================
levels = h.LevelList;
if numel(levels) >= 2
  v = levels(1:min(2,end));
else
  v = levels;
end
t2 = clabel(Cmat, h, v);
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t3 = clabel(C_only);
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t4 = clabel(C_only, levels(1:min(2,end)));
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t5 = clabel([], h);
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t6 = clabel(Cmat, h, 'FontSize', 12, 'Color', 'red');
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t7 = clabel(Cmat, h, 'LabelSpacing', 100);
%=============================================================================
fig = figure();
ax = axes('Parent',fig);
C_only = contour(ax, X, Y, Z); % returns contour matrix when assigned
t8 = clabel(Cmat, h, 'Visible', 'off');
%=============================================================================
