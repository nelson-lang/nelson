%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Apply element-wise and matrix operations to a small dataset.
A = [1, 2; 3, 4];
B = [2, 0; 1, 2];
disp('Matrix product:');
disp(A * B);
disp('Element-wise squares:');
disp(A .^ 2);
