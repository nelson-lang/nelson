%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isfalse(single(3) ~= single(3));
assert_istrue(single(3) ~= single(4));
assert_istrue(single(Inf) ~= single(-Inf));
%=============================================================================
R = single([1 2; 3 4]) ~= single(1);
REF = [false, true; true, true];
assert_isequal(R, REF);
%=============================================================================
