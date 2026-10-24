%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
A = int8([]);
R = evalc('A');
REF = '
A =

  0×0 empty int8 matrix

';
asserts.isequal(R, REF)
%=============================================================================
A = int8(zeros(0, 1));
R = evalc('A');
REF = '
A =

  0×1 empty int8 column vector

';
asserts.isequal(R, REF)
%=============================================================================
A = int8(zeros(0, 1, 1));
R = evalc('A');
REF = '
A =

  0×1 empty int8 column vector

';
asserts.isequal(R, REF)
%=============================================================================
A = int8(1);
R = evalc('A');
REF = '
A =

  int8

   1

';
asserts.isequal(R, REF)
%=============================================================================
A = int8([1 1]);
R = evalc('A');
REF = '
A =

  1×2 int8 row vector

   1   1

';
asserts.isequal(R, REF)
%=============================================================================
A = int8([1, 2]);
R = evalc('A');
REF = '
A =

  1×2 int8 row vector

   1   2

';
asserts.isequal(R, REF)
%=============================================================================
A = int8(eye(2, 2));
R = evalc('A');
REF =  '
A =

  2×2 int8 matrix

   1   0
   0   1

';
asserts.isequal(R, REF)
%=============================================================================
A = int8(ones(2, 1, 2));
R = evalc('A');
REF = '
  2×1×2 int8 array

A(:,:,1) =

   1
   1


A(:,:,2) =

   1
   1

';
asserts.isequal(R, REF)
%=============================================================================
A = ['hello'; 'olleh'];
R = evalc('A');
REF = '
A =

  2×5 char array

    ''hello''
    ''olleh''

';
asserts.isequal(R, REF)
%=============================================================================
A = logical([]);
R = evalc('A');
REF = '
A =

  0×0 empty logical array

';
asserts.isequal(R, REF)
%=============================================================================
A = [true, false];
R = evalc('A');
REF = '
A =

  1×2 logical array

     true    false

';
asserts.isequal(R, REF)
%=============================================================================
A = {'f'};
R = evalc('A');
REF =  '
A =

  1×1 cell array

    {''f''}

';
asserts.isequal(R, REF)
%=============================================================================
A = struct([]);
R = evalc('A');
REF = '
A = 

  0×0 empty struct array with no fields.

';
asserts.isequal(R, REF)
%=============================================================================
A = struct();
R = evalc('A');
REF = '
A = 

  struct with no fields.

';
asserts.isequal(R, REF)
%=============================================================================

