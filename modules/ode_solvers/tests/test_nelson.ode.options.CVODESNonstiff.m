%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = nelson.ode.options.CVODESNonstiff('MaxOrder', 12, 'NormControl', 'on', ...
  'LinearSolver', 'dense');
asserts.isequal(options.ID, 'cvodesnonstiff');
asserts.isequal(options.MaxOrder, 12);
asserts.isequal(options.NormControl, 'on');
asserts.isequal(options.LinearSolver, 'dense');
s = struct(options);
asserts.isequal(s.ID, 'cvodesnonstiff');
% a freshly constructed instance uses MaxOrder = 12 as its default
asserts.istrue(isDefault(nelson.ode.options.CVODESNonstiff()));
asserts.isfalse(isDefault(nelson.ode.options.CVODESNonstiff('MaxOrder', 8)));
asserts.checkerror('nelson.ode.options.CVODESNonstiff(''MaxOrder'', 13)', ...
  sprintf(_('MaxOrder must be an integer scalar between %d and %d.'), 1, 12));
%=============================================================================
