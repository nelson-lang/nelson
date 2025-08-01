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
format('bank')
%=============================================================================
R = evalc('A = single(complex([0.3 .2 .1],.4))');
REF = '
A =

  1×3 single row vector

          0.30          0.20          0.10

';
assert_isequal(R, REF)
%=============================================================================
