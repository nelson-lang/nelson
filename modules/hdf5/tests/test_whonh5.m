%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('whonh5'), 1);
asserts.isequal(nargout('whonh5'), 1);
%=============================================================================
test_whonh5_file = [tempname(), '.nh5'];
if (isfile(test_whonh5_file))
  rmfile(test_whonh5_file)
end
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savenh5(test_whonh5_file, 'A', 'B', 'C', 'D')
c = whonh5(test_whonh5_file);
REF = {'A'; 'B'; 'C'; 'D'}
asserts.isequal(c, REF);
%=============================================================================
c = whonh5(test_whonh5_file, 'A', 'C');
REF = {'A'; 'C'}
asserts.isequal(c, REF);
%=============================================================================
whonh5(test_whonh5_file, 'A', 'C');
whonh5(test_whonh5_file);
%=============================================================================
rmfile(test_whonh5_file);
%=============================================================================
