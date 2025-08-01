%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('factor'), 1);
assert_isequal(nargout('factor'), 1);
%=============================================================================
R = factor(200);
REF = [2     2     2     5     5];
assert_isequal(R, REF);
%=============================================================================
R = factor(single(200));
REF = single([2     2     2     5     5]);
assert_isequal(R, REF);
%=============================================================================
n = uint16(138);
R = factor(n);
REF = uint16([ 2    3   23]);
assert_isequal(R, REF);
%=============================================================================
R = factor(3);
REF = 3;
assert_isequal(R, REF);
%=============================================================================
