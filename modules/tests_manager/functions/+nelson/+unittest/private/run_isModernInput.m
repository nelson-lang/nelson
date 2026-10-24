%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = run_isModernInput(value)
  tf = isstruct(value) && isfield(value, 'type') ...
    && (strcmp(value.type, 'TestSuite') || strcmp(value.type, 'TestPlan'));
end
