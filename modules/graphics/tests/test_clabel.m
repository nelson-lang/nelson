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
fig = figure();
ax = axes('Parent', fig);
[X, Y, Z] = peaks(40);
[Cmat, h] = contour(ax, X, Y, Z);
t1 = clabel(Cmat, h);
% Labels are rendered as hidden-handle internal text objects.
children = ax.Children;
asserts.isequal(numel(children), 1);
asserts.isequal(children(1), h);
asserts.istrue(numel(allchild(ax)) > numel(children));
asserts.isequal(size(t1), [0, 0])
asserts.isequal(class(t1), 'nelson.graphics.GraphicsPlaceholder');
for i = 1:numel(t1)
  asserts.isequal(t1(i).Type, 'text');
end
%=============================================================================
