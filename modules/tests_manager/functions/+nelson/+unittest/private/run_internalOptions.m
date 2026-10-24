%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function internalOptions = run_internalOptions(options)
  internalOptions = struct('nelson_unittest_internal_options', true, ...
    'Timeout', options.Timeout, 'LauncherGrace', run_launcherGrace(options), ...
    'WorkerPoolInline', logical(options.WorkerPoolInline));
end
%=============================================================================
