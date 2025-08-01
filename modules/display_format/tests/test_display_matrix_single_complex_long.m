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
rng('default')
format('long')
%=============================================================================
R = evalc('A = single(complex(magic(3)*eps, eps))');
REF = '
A =

  3×3 single matrix

   1.0e-14 *

  0.1776357 + 0.0222045i  0.0222045 + 0.0222045i  0.1332268 + 0.0222045i
  0.0666134 + 0.0222045i  0.1110223 + 0.0222045i  0.1554312 + 0.0222045i
  0.0888178 + 0.0222045i  0.1998401 + 0.0222045i  0.0444089 + 0.0222045i

';
assert_isequal(R, REF);
%=============================================================================
