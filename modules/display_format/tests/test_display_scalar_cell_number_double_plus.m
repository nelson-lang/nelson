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
format('+')
%=============================================================================
R = evalc('A = {-1}');
REF =   '
A =

  1×1 cell array

    {[-]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {0}');
REF =   '
A =

  1×1 cell array

    {[]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {1}');
REF =   '
A =

  1×1 cell array

    {[+]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {NaN}');
REF =   '
A =

  1×1 cell array

    {[+]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-Inf}');
REF =   '
A =

  1×1 cell array

    {[-]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {Inf}');
REF =   '
A =

  1×1 cell array

    {[+]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(pi, -pi)}');
REF =   '
A =

  1×1 cell array

    {[+]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(-pi, pi)}');
REF =   '
A =

  1×1 cell array

    {[-]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1.8e99, -eps)}');
REF =   '
A =

  1×1 cell array

    {[+]}

';
assert_isequal(R, REF)
%=============================================================================
