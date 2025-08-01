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
format('hex')
%=============================================================================
R = evalc('A = {1}');
REF =    '
A =

  1×1 cell array

    {[3ff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {pi}');
REF =  '
A =

  1×1 cell array

    {[400921fb54442d18]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-pi}');
REF =  '
A =

  1×1 cell array

    {[c00921fb54442d18]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {NaN}');
REF = '
A =

  1×1 cell array

    {[fff8000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-Inf}');
REF =  '
A =

  1×1 cell array

    {[fff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {Inf}');
REF =  '
A =

  1×1 cell array

    {[7ff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {eps}');
REF = '
A =

  1×1 cell array

    {[3cb0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-eps}');
REF =   '
A =

  1×1 cell array

    {[bcb0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(pi, pi)}');
REF =  '
A =

  1×1 cell array

    {[400921fb54442d18   400921fb54442d18i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1, 0)}');
REF =  '
A =

  1×1 cell array

    {[3ff0000000000000   0000000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(NaN, NaN)}');
REF =  '
A =

  1×1 cell array

    {[fff8000000000000   fff8000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1.8e99, -eps)}');
REF =  '
A =

  1×1 cell array

    {[548a559d2be075aa   bcb0000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(ones(2,2), -eps)}');
REF =  '
A =

  1×1 cell array

    {2×2 double}

';
assert_isequal(R, REF)
%=============================================================================
