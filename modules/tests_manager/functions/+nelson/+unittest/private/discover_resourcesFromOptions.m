%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function resources = discover_resourcesFromOptions(options)
  resources = {};
  names = {'with_display', 'mpi_mode', 'audio_input_required', 'audio_output_required', ...
    'audio_required', 'excel_required', 'ipc_required', 'file_watcher_required', ...
    'c_cpp_compiler_required', 'python_environment_required', 'julia_environment_required'};
  for k = 1:length(names)
    if options.(names{k})
      resources = [resources; names{k}];
    end
  end
end
