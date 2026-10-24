%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = nelson.ode.options.IDAS('Vectorized', {'on', 'off'}, ...
  'ComputeConsistentInitialConditions', false);
asserts.isequal(options.ID, 'idas');
asserts.isequal(options.Vectorization, {'on', 'off'});
asserts.isfalse(options.ComputeConsistentInitialConditions);
s = struct(options);
asserts.isequal(s.ID, 'idas');
asserts.checkerror('nelson.ode.options.IDAS(''Vectorized'', {''on''})', ...
  sprintf(_('%s must be ''on'' or ''off''.'), 'Vectorized'));
%=============================================================================
