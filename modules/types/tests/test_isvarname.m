%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isvarname'), 1);
asserts.isequal(nargout('isvarname'), 1);
%=============================================================================
asserts.isfalse(isvarname(''))
asserts.isfalse(isvarname([]))
asserts.isfalse(isvarname('8t'))
asserts.isfalse(isvarname("8t"))
%=============================================================================
asserts.istrue(isvarname('t9'))
asserts.istrue(isvarname("t9"))
%=============================================================================
% language keywords are not valid variable names
assert_isfalse(isvarname('for'));
assert_isfalse(isvarname('while'));
assert_isfalse(isvarname('end'));
assert_isfalse(isvarname('function'));
assert_isfalse(isvarname('if'));
%=============================================================================
% keyword matching is case sensitive and only matches the exact keyword
assert_istrue(isvarname('For'));
assert_istrue(isvarname('forx'));
assert_istrue(isvarname('endfor'));
%=============================================================================
