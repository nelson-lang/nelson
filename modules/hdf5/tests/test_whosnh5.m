%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('whosnh5'), 1);
asserts.isequal(nargout('whosnh5'), 1);
%=============================================================================
test_whosnh5_1_file = [tempname(), '.nh5'];
test_whosnh5_file = [tempname(), '.nh5'];
D = sparse(3i);
savenh5(test_whosnh5_1_file, 'D');
%=============================================================================
S = whosnh5(test_whosnh5_1_file);
asserts.isequal(S.name, 'D');
asserts.isequal(S.size, [1 1]);
asserts.isequal(S.bytes, NaN);
asserts.isequal(S.class, 'sparsedouble');
asserts.isequal(S.global, false);
asserts.isequal(S.sparse, true);
asserts.isequal(S.complex, true);
asserts.isequal(S.nesting, struct('function', '', 'level', 0));
asserts.isequal(S.persistent, false);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savenh5(test_whosnh5_file, 'A', 'B', 'C', 'D');
whosnh5(test_whosnh5_file)
%=============================================================================
rmfile(test_whosnh5_1_file);
rmfile(test_whosnh5_file);
%=============================================================================
