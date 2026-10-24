%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = tuneWeights_isResult(value)
  tf = isstruct(value) && isfield(value, 'type') && strcmp(value.type, 'TestRunResult');
end
%=============================================================================
