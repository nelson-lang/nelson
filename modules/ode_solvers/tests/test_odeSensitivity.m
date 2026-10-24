%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sensitivity = odeSensitivity('Method', 'forward', 'ParameterIndices', [2 1], ...
  'ObjectiveTime', 1, 'AdjointRelativeTolerance', 1e-4);
asserts.isequal(sensitivity.Method, 'direct');
asserts.isequal(sensitivity.ParameterIndices, [2; 1]);
asserts.isequal(sensitivity.ObjectiveTime, 1);
asserts.isequal(sensitivity.AdjointRelativeTolerance, 1e-4);
asserts.checkerror('odeSensitivity(''ParameterIndices'', 0)', ...
  _('ParameterIndices must contain positive integer values.'));
%=============================================================================
