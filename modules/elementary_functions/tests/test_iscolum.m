%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('iscolumn'), 1)
asserts.isequal(nargout('iscolumn'), 1)
%=============================================================================
s.c = 1;
asserts.istrue(iscolumn(s))
%=============================================================================
asserts.istrue(iscolumn("n"))
asserts.istrue(iscolumn('n'))
asserts.istrue(iscolumn("nelson"))
asserts.isfalse(iscolumn('nelson'))
asserts.istrue(iscolumn(["test"; "ing"]))
asserts.isfalse(iscolumn(["test", "ing"]))
%=============================================================================
asserts.isfalse(iscolumn([1, 2, 3]))
asserts.istrue(iscolumn([1; 2; 3]))
asserts.istrue(iscolumn(1))
asserts.isfalse(iscolumn([]))
asserts.isfalse(iscolumn([1, 2; 3, 4]))
%=============================================================================

