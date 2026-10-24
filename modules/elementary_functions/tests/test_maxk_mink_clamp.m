%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% k larger than the number of elements: return all elements sorted
asserts.isequal(maxk([1 3 2], 5), [3 2 1]);
asserts.isequal(mink([1 3 2], 5), [1 2 3]);
asserts.isequal(maxk([1; 3; 2], 5), [3; 2; 1]);
asserts.isequal(mink([1; 3; 2], 5), [1; 2; 3]);
% matrices: clamp along requested dimension
asserts.isequal(maxk(magic(3), 5), sort(magic(3), 1, 'descend'));
asserts.isequal(mink(magic(3), 5), sort(magic(3), 1, 'ascend'));
% normal cases unchanged
asserts.isequal(maxk([5 1 9 4], 2), [9 5]);
asserts.isequal(mink([5 1 9 4], 2), [1 4]);
[V, I] = maxk([5 1 9 4], 2);
asserts.isequal(I, [3 1]);
%=============================================================================
