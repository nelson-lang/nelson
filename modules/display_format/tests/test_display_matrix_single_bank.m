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
R = evalc('A = single([0.333 .2222 .1111 .4444])');
REF = '
A =

  1×4 single row vector

          0.33          0.22          0.11          0.44

';
assert_isequal(R, REF)
%=============================================================================
