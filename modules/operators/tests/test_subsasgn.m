%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = 5:10;
R = subsasgn(A, substruct('()', {2}), 1);
REF =  [5     1     7     8     9    10];
assert_isequal(R, REF);
%=============================================================================
A = magic(3);
R = subsasgn(A, substruct('()', {2, 3}), 10);
REF = [   8     1     6;3     5     10;4     9     2];
assert_isequal(R, REF);
%=============================================================================
A = magic(3);
R = subsasgn(A, substruct('()', {':'}), 10);
REF = [   10     10     10;10     10     10;10     10     10];
assert_isequal(R, REF);
%=============================================================================
A = [];
A.a = 33;
R = subsasgn(A, substruct('.', 'a'), 10);
REF = [];
REF.a = 10;
assert_isequal(R, REF);
%=============================================================================
A = [];
A.a = 33;
R = subsasgn(A, substruct('.', 'b'), 10);
REF = [];
REF.a = 33;
REF.b = 10;
assert_isequal(R, REF);
%=============================================================================
A = {1,[1:3];'a',true};
R = subsasgn(A, substruct('{}', {1, 2}), 23);
REF = {1, 23; 'a', true};
assert_isequal(R, REF);
%=============================================================================
A = dir();
assert_checkerror('R = subsasgn(A, substruct(''()'', {3}, ''.'',''name''), ''f'');', _('No support for index chaining.'));
%=============================================================================