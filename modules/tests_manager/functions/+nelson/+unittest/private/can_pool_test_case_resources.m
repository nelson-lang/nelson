%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = can_pool_test_case_resources(options)
  tf = ~options.sequential_test_required && ~options.ipc_required ...
    && ~options.file_watcher_required && ~options.audio_input_required ...
    && ~options.audio_output_required && ~options.audio_required ...
    && ~options.english_imposed ...
    && ~options.python_environment_required ...
    && ~options.julia_environment_required;
end
%=============================================================================
