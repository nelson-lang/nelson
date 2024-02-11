%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
f.ToolBar = 'none';
[X, Y, Z] = peaks();
surf(X, Y, Z);
ax = gca();
% To change
assert_isapprox(ax.CameraPosition, [-44.5633  -58.0760   43.0281], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
view(90,0)
assert_isapprox(ax.CameraPosition, [ 84.5277   -0.0000    0.7643], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
view(2)
assert_isapprox(ax.CameraPosition, [ 0         0   14.0880], 1e-4);
assert_isequal(ax.CameraUpVector, [0     6     0])
%=============================================================================
