%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = MException('Nelson:test:missingArgument', 'Missing argument.');
correction = nelson.lang.correction.AppendArgumentsCorrection('value');
%=============================================================================
withCorrection = addCorrection(base, correction);
asserts.isequal(class(withCorrection), 'MException');
asserts.isequal(class(withCorrection.Correction), ...
  'nelson.lang.correction.AppendArgumentsCorrection');
asserts.isequal(withCorrection.Correction.Arguments, 'value');
asserts.isequal(base.Correction, []);
%=============================================================================
withCorrection = base.addCorrection(correction);
asserts.isequal(class(withCorrection.Correction), ...
  'nelson.lang.correction.AppendArgumentsCorrection');
%=============================================================================
