%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(or(true, true));
asserts.istrue(or(true, false));
asserts.isfalse(or(false, false));
asserts.istrue(or(false, true));
%=============================================================================
asserts.istrue(true | true);
asserts.istrue(true | false);
asserts.isfalse(false | false);
asserts.istrue(false | true);
%=============================================================================
A = [50 70 0; 0 20 90; 50 0 0];
B = [60 60 0; 10 30 50; -10 0 0];
R1 = or(A, B);
R2 = A | B;
asserts.isequal(R1, R2);
REF = [true, true, false; true, true, true; true, false, false];
asserts.isequal(R1, REF);
%=============================================================================
A = [true, false];
B = [true; false];
R1 = or(A, B);
R2 = A | B;
asserts.isequal(R1, R2);
REF = [true, true; true, false];
asserts.isequal(R1, REF);
%=============================================================================
A = [true false, true];
B = [true; false];
R1 = or(A, B);
R2 = A | B;
asserts.isequal(R1, R2);
REF = [true, true, true; true, false, true];
asserts.isequal(R1, REF);
%=============================================================================
A = [true false, true];
B = [true; false];
R1 = or(B, A);
R2 = B | A;
asserts.isequal(R1, R2);
REF = logical([1 1 1; 1 0 1]);
asserts.isequal(R1, REF);
%=============================================================================
A = [true, false, true];
B = [true, false, true];
R1 = or(B, A);
R2 = B | A;
asserts.isequal(R1, R2);
REF = [true, false, true];
asserts.isequal(R1, REF);
%=============================================================================
A = zeros(3, 2, 2);
R = A | A;
REF = logical(A);
asserts.isequal(R, REF);
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
asserts.checkerror('A | B', [_('Size mismatch on arguments to'), ' ', '|']);
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
asserts.checkerror('B | A', [_('Size mismatch on arguments to'), ' ', '|']);
%=============================================================================
A = ones(1, 2, 3);
B = [1:3];
asserts.checkerror('A | B', [_('Size mismatch on arguments to'), ' ', '|']);
%=============================================================================
R = [true, false] | true;
REF = [true, true];
asserts.isequal(R, REF);
%=============================================================================
R = [true, false] | false;
REF = [true, false];
asserts.isequal(R, REF);
%=============================================================================
