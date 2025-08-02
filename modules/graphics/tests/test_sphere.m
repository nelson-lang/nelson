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
colormap(gray);
subplot(1, 3, 1);
ax1 = gca();
sphere(ax1);
axis equal
title('20-by-20 faces (Default)')
subplot(1, 3, 2);
ax2 = gca();
sphere(ax2, 50)
axis equal
title('50-by-50 faces')
subplot(1, 3, 3);
ax3 = gca();
sphere(ax3,100)
axis equal
title('100-by-100 faces')
%=============================================================================
[X, Y, Z] = sphere(2);
X_REF = [0     0     0; -1     1    -1; 0     0     0];
Y_REF = [0     0     0;  0     0     0; 0     0     0];
Z_REF = [-1    -1    -1; 0     0     0;  1     1     1];
assert_isapprox(X, X_REF, 1e-4);
assert_isapprox(Y, Y_REF, 1e-4);
assert_isapprox(Z, Z_REF, 1e-4);
%=============================================================================
