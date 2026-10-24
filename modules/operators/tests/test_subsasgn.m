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
REF = [5 1 7 8 9 10];
asserts.isequal(R, REF);
%=============================================================================
A = magic(3);
R = subsasgn(A, substruct('()', {2, 3}), 10);
REF = [8 1 6; 3 5 10; 4 9 2];
asserts.isequal(R, REF);
%=============================================================================
A = magic(3);
R = subsasgn(A, substruct('()', {':'}), 10);
REF = [10 10 10; 10 10 10; 10 10 10];
asserts.isequal(R, REF);
%=============================================================================
A = [];
A.a = 33;
R = subsasgn(A, substruct('.', 'a'), 10);
REF = [];
REF.a = 10;
asserts.isequal(R, REF);
%=============================================================================
A = [];
A.a = 33;
R = subsasgn(A, substruct('.', 'b'), 10);
REF = [];
REF.a = 33;
REF.b = 10;
asserts.isequal(R, REF);
%=============================================================================
A = {1, [1:3]; 'a', true};
R = subsasgn(A, substruct('{}', {1, 2}), 23);
REF = {1, 23; 'a', true};
asserts.isequal(R, REF);
%=============================================================================
A = [];
for k = 1:32
  A(k) = k;
end
asserts.isequal(A, 1:32);
asserts.isequal(size(A), [1 32]);
B = A;
A(48) = 99;
asserts.isequal(B, 1:32);
asserts.isequal(A(1:32), 1:32);
asserts.isequal(A(33:47), zeros(1, 15));
asserts.isequal(A(48), 99);
asserts.isequal(size(A), [1 48]);
%=============================================================================
A = zeros(1, 12);
B = A;
A(4:9) = 11:16;
asserts.isequal(B, zeros(1, 12));
asserts.isequal(A, [0 0 0 11 12 13 14 15 16 0 0 0]);
A(14:17) = 21:24;
asserts.isequal(A(13), 0);
asserts.isequal(A(14:17), 21:24);
asserts.isequal(size(A), [1 17]);
%=============================================================================
A = dir();
asserts.checkerror('R = subsasgn(A, substruct(''()'', {3}, ''.'',''name''), ''f'');', _('No support for index chaining.'));
%=============================================================================
