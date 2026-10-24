%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isrow'), 1)
asserts.isequal(nargout('isrow'), 1)
%=============================================================================
s.c = 1;
asserts.istrue(isrow(s))
%=============================================================================
asserts.istrue(isrow("n"))
asserts.istrue(isrow('n'))
asserts.istrue(isrow("nelson"))
asserts.istrue(isrow('nelson'))
asserts.isfalse(isrow(["test"; "ing"]))
asserts.istrue(isrow(["test", "ing"]))
%=============================================================================
asserts.istrue(isrow([1, 2, 3]))
asserts.isfalse(isrow([1; 2; 3]))
asserts.istrue(isrow(1))
asserts.isfalse(isrow([]))
asserts.isfalse(isrow([1, 2; 3, 4]))
%=============================================================================

