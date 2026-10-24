%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('bounds'), -2);
asserts.isequal(nargout('bounds'), 2);
%=============================================================================
A = [3 1; 2 4; 5 0];
[smallest, largest] = bounds(A);
asserts.isequal(smallest, [2 0]);
asserts.isequal(largest, [5 4]);
%=============================================================================
[smallest, largest] = bounds(A, 2);
asserts.isequal(smallest, [1; 2; 0]);
asserts.isequal(largest, [3; 4; 5]);
%=============================================================================
B = [3 1; 2 4; 5 0];
[smallest, largest] = bounds(B);
asserts.isequal(smallest, [2 0]);
asserts.isequal(largest, [5 4]);
%=============================================================================
