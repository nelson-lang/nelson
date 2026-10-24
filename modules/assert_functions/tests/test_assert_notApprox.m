%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.notApprox'), -3);
asserts.isequal(nargout('asserts.notApprox'), 2);
%=============================================================================
asserts.notApprox(1, 2, eps);
asserts.notApprox(1, 2, eps, 0);
%=============================================================================
[r, msg] = asserts.notApprox(1, 1, eps);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.notApprox'));
%=============================================================================
asserts.checkerror('feval(''asserts.notApprox'', 1, 1, eps)', 'Assertion failed: asserts.notApprox expected values to be different.');
%=============================================================================
