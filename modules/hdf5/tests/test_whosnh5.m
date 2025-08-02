%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('whosnh5'), 1);
assert_isequal(nargout('whosnh5'), 1);
%=============================================================================
D = sparse(3i);
savenh5([tempdir(), 'test_whosnh5_1.nh5'],  'D');
%=============================================================================
S = whosnh5([tempdir(), 'test_whosnh5_1.nh5']);
assert_isequal(S.name, 'D');
assert_isequal(S.size, [1 1]);
assert_isequal(S.bytes, NaN);
assert_isequal(S.class, 'sparsedouble');
assert_isequal(S.global, false);
assert_isequal(S.sparse, true);
assert_isequal(S.complex, true);
assert_isequal(S.nesting, struct('function', '', 'level', 0));
assert_isequal(S.persistent, false);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savenh5([tempdir(), 'test_whosnh5.nh5'], 'A', 'B', 'C', 'D');
whosnh5([tempdir(), 'test_whosnh5.nh5'])
%=============================================================================
