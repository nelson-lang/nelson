%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('whonh5'), 1);
assert_isequal(nargout('whonh5'), 1);
%=============================================================================
if (isfile([tempdir(), 'test_whonh5.nh5']))
  rmfile([tempdir(), 'test_whonh5.nh5'])
end
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savenh5([tempdir(), 'test_whonh5.nh5'], 'A', 'B', 'C', 'D')
c = whonh5([tempdir(), 'test_whonh5.nh5']);
REF = {'A'; 'B'; 'C'; 'D'}
assert_isequal(c, REF);
%=============================================================================
c = whonh5([tempdir(), 'test_whonh5.nh5'], 'A', 'C');
REF = {'A'; 'C'}
assert_isequal(c, REF);
%=============================================================================
whonh5([tempdir(), 'test_whonh5.nh5'], 'A', 'C');
whonh5([tempdir(), 'test_whonh5.nh5']);
%=============================================================================
