%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.satisfies'), 2);
asserts.isequal(nargout('asserts.satisfies'), 2);
%=============================================================================
asserts.satisfies(1, 'isnumeric');
asserts.satisfies(1, @(x) isscalar(x));
%=============================================================================
[r, msg] = asserts.satisfies([1 2], @(x) isscalar(x));
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.satisfies'));
%=============================================================================
