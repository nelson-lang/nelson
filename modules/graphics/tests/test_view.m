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
assert_isapprox(ax.CameraPosition, [-5.7269   -7.4634    5.4314], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
view(90,0)
assert_isapprox(ax.CameraPosition, [10.6301   -0.0000    1.0002], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
view(2)
assert_isapprox(ax.CameraPosition, [ 0   -0.0000   11.7238], 1e-4);
assert_isequal(ax.CameraUpVector, [0     1     0])
%=============================================================================
