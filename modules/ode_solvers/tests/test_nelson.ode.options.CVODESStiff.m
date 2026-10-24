%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = nelson.ode.options.CVODESStiff('MaxOrder', 5, 'Vectorized', 'on', ...
  'Preconditioner', 'none');
asserts.isequal(options.ID, 'cvodesstiff');
asserts.isequal(options.MaxOrder, 5);
asserts.isequal(options.Vectorization, 'on');
asserts.isequal(options.Preconditioner, 'none');
s = struct(options);
asserts.isequal(s.ID, 'cvodesstiff');
asserts.checkerror('nelson.ode.options.CVODESStiff(''MaxOrder'', 6)', ...
  sprintf(_('MaxOrder must be an integer scalar between %d and %d.'), 1, 5));
%=============================================================================
