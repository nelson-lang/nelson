%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(and(true, true));
assert_isfalse(and(true, false));
assert_isfalse(and(false, false));
assert_isfalse(and(false, true));
%=============================================================================
assert_istrue(true & true);
assert_isfalse(true & false);
assert_isfalse(false & false);
assert_isfalse(false & true);
%=============================================================================
A = [50 70 0; 0 20 90; 50 0 0];
B = [60 60 0; 10 30 50; -10 0 0];
R1 = and(A, B);
R2 = A & B;
assert_isequal(R1, R2);
REF =  [true, true, false; false, true, true;  true, false, false];
assert_isequal(R1, REF);
%=============================================================================
A = [true, false];
B = [true; false];
R1 = and(A, B);
R2 = A & B;
assert_isequal(R1, R2);
REF =  [true, false;  false, false];
assert_isequal(R1, REF);
%=============================================================================
A = [true false, true];
B = [true; false];
R1 = and(A, B);
R2 = A & B;
assert_isequal(R1, R2);
REF =  [true, false,  true; false, false, false];
assert_isequal(R1, REF);
%=============================================================================
A = [true false, true];
B = [true; false];
R1 = and(B, A);
R2 = B & A;
assert_isequal(R1, R2);
REF =  [true, false,  true; false, false, false];
assert_isequal(R1, REF);
%=============================================================================
A = [true, false, true];
B = [true, false, true];
R1 = and(B, A);
R2 = B & A;
assert_isequal(R1, R2);
REF =  [true, false,  true];
assert_isequal(R1, REF);
%=============================================================================
A = zeros(3, 2, 2);
R = A & A;
REF = logical(A);
assert_isequal(R, REF);
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
assert_checkerror('A & B', [_('Size mismatch on arguments to '), '&']);
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
assert_checkerror('B & A', [_('Size mismatch on arguments to '), '&']);
%=============================================================================
A = ones(1,2,3);
B = [1:3];
assert_checkerror('A & B', [_('Size mismatch on arguments to '), '&']);
%=============================================================================
R = [true, false] & true;
REF = [true, false];
assert_isequal(R, REF);
%=============================================================================
R = [true, false] & false;
REF = [false, false];
assert_isequal(R, REF);
%=============================================================================
