%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('abcdchk'), -1);
assert_isequal(nargout('abcdchk'), -1);
%=============================================================================
[msg, A] = abcdchk(ones(3, 3));
A_REF = ones(3, 3);
MSG_REF.message = '';
MSG_REF.identifier = '';
MSG_REF = MSG_REF(zeros(0, 1));
assert_isequal(A, A_REF);
assert_isequal(msg, MSG_REF);
%=============================================================================