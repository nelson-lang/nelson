%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = int8([9 2; 9 5]);
R = unique(A);
REF = int8([2; 5; 9]);
asserts.isequal(R, REF);
%=============================================================================
A = int8([9 2; 9 5]);
[R, ia] = unique(A);
REF = int8([2; 5; 9]);
asserts.isequal(R, REF);
ia_REF = [3; 4; 1];
asserts.isequal(ia, ia_REF);
%=============================================================================
A = int8([9 2; 9 5]);
[R, ia, ic] = unique(A);
REF = int8([2; 5; 9]);
asserts.isequal(R, REF);
ia_REF = [3; 4; 1];
asserts.isequal(ia, ia_REF);
ic_REF = [3; 3; 1; 2];
asserts.isequal(ic, ic_REF);
%=============================================================================
A = int8(magic(6));
R = unique(A);
REF = int8([1:36]');
asserts.isequal(R, REF);
%=============================================================================
A = int8(magic(6));
[R, ia] = unique(A);
REF = int8([1:36]');
asserts.isequal(R, REF);
ia_REF = [7; 15; 2; 6; 11; 13; 14; 4; 9; 28; 36; 23; 24; 29; 34; 35; 22; 30; 25;
  33; 20; 21; 26; 31; 32; 19; 27; 10; 18; 5; 3; 8; 16; 17; 1; 12];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C, ia, ic] = unique(uint8([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = uint8([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
ic_REF = 1;
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(uint8([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = uint8([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
ia_REF = 1;
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(uint8([10, 20, 20, 30, 20, 40]), 'rows');
C_REF = uint8([10 20 20 30 20 40]);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(uint8([10, 20, 20, 30, 20, 40]));
C_REF = uint8([10 20 30 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 2; 3; 2; 4];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(uint8([10, 20, 20, 30, 20, 40]));
C_REF = uint8([10 20 30 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(uint8([10, 20, 20, 30, 20, 40]));
C_REF = uint8([10 20 30 40]);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(uint8([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 2; 3; 2; 4];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(uint8([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(uint8([10, 20, 20, 30, 20, 40]'), 'rows');
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
%=============================================================================
[C, ia, ic] = unique(uint8([10, 20, 20, 30, 20, 40]'));
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
ic_REF = [1; 2; 2; 3; 2; 4];
asserts.isequal(ic, ic_REF);
%=============================================================================
[C, ia] = unique(uint8([10, 20, 20, 30, 20, 40]'));
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
ia_REF = [1; 2; 4; 6];
asserts.isequal(ia, ia_REF);
%=============================================================================
[C] = unique(uint8([10, 20, 20, 30, 20, 40]'));
C_REF = uint8([10; 20; 30; 40]);
asserts.isequal(C, C_REF);
%=============================================================================
