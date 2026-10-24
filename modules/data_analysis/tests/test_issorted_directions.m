%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% strict directions
asserts.isfalse(issorted([1 1 2], 'strictascend'));
asserts.istrue(issorted([1 2 3], 'strictascend'));
asserts.isfalse(issorted([3 2 2], 'strictdescend'));
asserts.istrue(issorted([3 2 1], 'strictdescend'));
asserts.isfalse(issorted(int8([1 1 2]), 'strictascend'));
%=============================================================================
% monotonic
asserts.istrue(issorted([3 2 1], 'monotonic'));
asserts.istrue(issorted([1 2 3], 'monotonic'));
asserts.isfalse(issorted([1 2 1], 'monotonic'));
asserts.istrue(issorted([1 1 2], 'monotonic'));
asserts.isfalse(issorted([1 1 2], 'strictmonotonic'));
asserts.istrue(issorted([3 2 1], 'strictmonotonic'));
%=============================================================================
% dim + direction
asserts.istrue(issorted([1 2 3], 2, 'ascend'));
asserts.istrue(issorted([5 4 3], 2, 'descend'));
asserts.isfalse(issorted([3 1; 2 2], 2));
asserts.istrue(issorted([1 2; 3 4], 2));
%=============================================================================
% NaN placement: last for 'ascend', first for 'descend',
% any NaN makes strict directions false
asserts.istrue(issorted([1 NaN]));
asserts.isfalse(issorted([NaN 1]));
asserts.isfalse(issorted([1 NaN 2]));
asserts.istrue(issorted([NaN NaN]));
asserts.istrue(issorted([1 NaN NaN]));
asserts.istrue(issorted([NaN 2 1], 'descend'));
asserts.isfalse(issorted([2 1 NaN], 'descend'));
asserts.isfalse(issorted([1 Inf NaN], 'strictascend'));
asserts.istrue(issorted([1 2 NaN], 'monotonic'));
asserts.istrue(issorted([NaN 2 1], 'monotonic'));
asserts.isfalse(issorted([NaN 1 2], 'monotonic'));
%=============================================================================
% basic behavior kept
asserts.istrue(issorted([1 2 3]));
asserts.isfalse(issorted([3 2 1]));
asserts.istrue(issorted([3 2 1], 'descend'));
asserts.istrue(issorted([Inf Inf]));
%=============================================================================
