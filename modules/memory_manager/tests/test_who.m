%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('who'), 1);
assert_isequal(nargout('who'), 1);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
save([tempdir(), 'test_who.mat'], 'A', 'B', 'C', 'D')
save([tempdir(), 'test_who.nh5'], 'A', 'B', 'C', 'D')
%=============================================================================
who('-file', [tempdir(), 'test_who.mat'])
c = who('-file', [tempdir(), 'test_who.mat']);
REF = {'A'; 'B'; 'C'; 'D'}
assert_isequal(c, REF);
%=============================================================================
who('-file', [tempdir(), 'test_who.mat'], 'A', 'C')
c = who('-file', [tempdir(), 'test_who.mat'], 'A', 'C');
REF = {'A'; 'C'}
assert_isequal(c, REF);
%=============================================================================
who('-file', [tempdir(), 'test_who.nh5'])
c = who('-file', [tempdir(), 'test_who.nh5']);
REF = {'A'; 'B'; 'C'; 'D'}
assert_isequal(c, REF);
%=============================================================================
who('-file', [tempdir(), 'test_who.nh5'], 'A', 'C')
c = who('-file', [tempdir(), 'test_who.nh5'], 'A', 'C');
REF = {'A'; 'C'}
assert_isequal(c, REF);
%=============================================================================
who()
who('global')
%=============================================================================
