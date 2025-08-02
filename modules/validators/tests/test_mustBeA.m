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
assert_isequal(nargin('mustBeA'), -2);
assert_isequal(nargout('mustBeA'), 0);
%=============================================================================
A = 3;
mustBeA(A, 'double')
mustBeA(A, {'double'})
mustBeA(A, {'double', 'single'})
mustBeA(A, ["double", "single"])
%=============================================================================
assert_checkerror('mustBeA(A, ''single'')', 'Value must be one of the following types: ''single''.', 'Nelson:validators:mustBeA');
%=============================================================================
