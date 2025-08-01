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
[x, y, z] = cylinder();
REF_X = [1     0.95106     0.80902     0.58779     0.30902  6.1232e-17    -0.30902    -0.58779    -0.80902    -0.95106          -1    -0.95106    -0.80902    -0.58779    -0.30902  -1.837e-16     0.30902     0.58779     0.80902     0.95106           1;
1     0.95106     0.80902     0.58779     0.30902  6.1232e-17    -0.30902    -0.58779    -0.80902    -0.95106          -1    -0.95106    -0.80902    -0.58779    -0.30902  -1.837e-16     0.30902     0.58779     0.80902     0.95106           1];
assert_isapprox(x, REF_X, 1e-5);
REF_Z =  [0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0;
1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1];
assert_isequal(z, REF_Z);
REF_Y = [0     0.30902     0.58779     0.80902     0.95106           1     0.95106     0.80902     0.58779     0.30902  1.2246e-16    -0.30902    -0.58779    -0.80902    -0.95106          -1    -0.95106    -0.80902    -0.58779    -0.30902           0;
0     0.30902     0.58779     0.80902     0.95106           1     0.95106     0.80902     0.58779     0.30902  1.2246e-16    -0.30902    -0.58779    -0.80902    -0.95106          -1    -0.95106    -0.80902    -0.58779    -0.30902           0];
%=============================================================================
cylinder();
%=============================================================================
