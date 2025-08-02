%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('rmfield'), 2);
assert_isequal(nargout('rmfield'), 1);
%=============================================================================
S.first = 1;
S.second = 2;
S.third = 3;
S.fourth = 4;
fields = {'second','fourth'};
R = rmfield(S, fields);
REF.first = 1;
REF.third = 3;
assert_isequal(R, REF);
%=============================================================================
A(1,1).first = 1;
A(1,1).second = 2;
A(1,1).third = 3;
A(2,1).first = 4;
A(2,1).second = 5;
A(2,1).third = 6;
A(2,2).first = 7;
A(2,2).second = 8;
A(2,2).third = 9;
R = rmfield(A, 'second');
REF2(1,1).first = 1;
REF2(1,1).third = 3;
REF2(2,1).first = 4;
REF2(2,1).third = 6;
REF2(2,2).first = 7;
REF2(2,2).third = 9;
assert_isequal(R, REF2);
%=============================================================================
