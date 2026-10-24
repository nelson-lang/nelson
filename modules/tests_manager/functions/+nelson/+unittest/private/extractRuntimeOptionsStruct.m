%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function runtimeOptions = extractRuntimeOptionsStruct(runtimeOptions, value)
  runtimeOptions.Timeout = getOptionField(value, 'Timeout', runtimeOptions.Timeout);
  runtimeOptions.LauncherGrace = getOptionField(value, 'LauncherGrace', runtimeOptions.LauncherGrace);
  runtimeOptions.WorkerPoolInline = logical(getOptionField(...
    value, 'WorkerPoolInline', runtimeOptions.WorkerPoolInline));
end
%=============================================================================
