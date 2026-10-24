%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression test: a sparse double input to betainc must not be read as a
% dense Eigen buffer (operator precedence bug: '&& !isSparse()' only guarded
% the SINGLE branch, so a sparse double slipped through -> out-of-bounds read).
% It must now route to overload and error cleanly, like gammainc does.
%=============================================================================
asserts.checkerror('betainc(sparse(0.5), 1, 2)', sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'betainc'));
%=============================================================================
asserts.checkerror('betainc(0.5, sparse(1), 2)', sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'betainc'));
%=============================================================================
asserts.checkerror('betainc(0.5, 1, sparse(2))', sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'betainc'));
%=============================================================================
% dense inputs still compute correctly (unchanged behavior).
R = betainc(full(0.5), 1, 2);
REF = 0.75;
asserts.isapprox(R, REF, 1e-4);
%=============================================================================
