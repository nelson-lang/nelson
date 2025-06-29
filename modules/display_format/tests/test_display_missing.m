%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
R = evalc('A = missing');
REF =  '
A =

  missing

    <missing>

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = repmat(missing, 0, 3)');
REF = '
A =

  0×3 empty missing array

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = repmat(missing, 3, 2)');
REF =  '
A =

  3×2 missing array

    <missing>    <missing>
    <missing>    <missing>
    <missing>    <missing>

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = repmat(missing, 3, 2, 2)');
REF =  '
  3×2×2 missing array

A(:,:,1) =

    <missing>    <missing>
    <missing>    <missing>
    <missing>    <missing>


A(:,:,2) =

    <missing>    <missing>
    <missing>    <missing>
    <missing>    <missing>

';
assert_isequal(R, REF);
%=============================================================================
R =  evalc('A = {missing}');
REF =  '
A =

  1×1 cell array

    {[<missing>]}

';
assert_isequal(R, REF);
%=============================================================================
R =  evalc('A = {1,repmat(missing,3,2)}');
REF = '
A =

  1×2 cell array

    {[1]}    {3×2 missing}

';
assert_isequal(R, REF);
%=============================================================================
R =  evalc('B = {}; B.a = missing; B.b = {missing}; B.c = repmat(missing,3,2); B.d = {repmat(missing,3,2)}');
REF = '
B =

  struct with fields:

    a: <missing>
    b: {<missing>}
    c: [3×2 missing]
    d: {[3×2 missing]}

';
assert_isequal(R, REF);
%=============================================================================
