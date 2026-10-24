%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
jac = odeJacobian(@(t, y) - 1, 'Pattern', true(1), 'Constant', 'on');
asserts.istrue(isa(jac.Jacobian, 'function_handle'));
asserts.isequal(jac.SparsityPattern, true(1));
asserts.isequal(jac.Constant, 'on');
asserts.checkerror('odeJacobian({1})', sprintf(_('%s must be numeric or a function handle.'), 'Jacobian'));
asserts.checkerror('odeJacobian([], ''Constant'', ''maybe'')', ...
  _('Jacobian constant flag must be ''on'' or ''off''.'));
%=============================================================================
