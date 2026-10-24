%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% scalar N applies to the first non-singleton dimension
asserts.isequal(circshift([1 2 3 4], 1), [4 1 2 3]);
asserts.isequal(circshift([1 2 3 4], -1), [2 3 4 1]);
asserts.isequal(circshift([1; 2; 3], 1), [3; 1; 2]);
asserts.isequal(circshift([1 2 3 4], 6), [3 4 1 2]);
% matrices: first non-singleton dimension is 1 (unchanged behavior)
asserts.isequal(circshift(magic(3), 1), circshift(magic(3), [1 0]));
asserts.isequal(circshift(magic(3), 1), [4 9 2; 8 1 6; 3 5 7]);
% explicit dimension still works
asserts.isequal(circshift(magic(3), 1, 2), circshift(magic(3), [0 1]));
% vector N unchanged
asserts.isequal(circshift(magic(3), [1 1]), [2 4 9; 6 8 1; 7 3 5]);
% scalar unchanged
asserts.isequal(circshift(5, 1), 5);
%=============================================================================
