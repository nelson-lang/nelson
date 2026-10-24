%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = run_tagOptionsFromPlanCase(planCase)
  names = {'not_fixed', 'interactive_test', 'with_display', 'cli_mode', ...
    'gui_mode', 'adv_cli_mode', 'check_ref', 'english_imposed', ...
    'windows_only', 'mac_only', 'unix_only', 'release_only', ...
    'excel_required', 'mpi_mode', 'audio_input_required', ...
    'audio_output_required', 'audio_required', 'c_cpp_compiler_required', ...
    'index_64_bit_required', 'no_user_modules', 'ipc_required', ...
    'sequential_test_required', 'native_architecture_required', ...
    'file_watcher_required', 'python_environment_required', ...
    'julia_environment_required', 'reuse_process'};
  options = run_emptyTagOptions(names);
  for k = 1:length(names)
    options.(names{k}) = run_planCaseHasTag(planCase, names{k});
  end
  options.weight = 1;
  if isfield(planCase, 'weight')
    options.weight = planCase.weight;
  end
  options.timeout = 0;
  if isfield(planCase, 'timeout')
    options.timeout = planCase.timeout;
  end
  options.cli_mode = strcmp(planCase.mode, 'cli') || options.cli_mode;
  options.gui_mode = strcmp(planCase.mode, 'gui') || options.gui_mode;
  options.adv_cli_mode = strcmp(planCase.mode, 'adv_cli') || options.adv_cli_mode;
end
%=============================================================================
function options = run_emptyTagOptions(names)
  options = struct();
  for k = 1:length(names)
    options.(names{k}) = false;
  end
end
%=============================================================================
function tf = run_planCaseHasTag(planCase, name)
  tf = run_planCaseHasValue(planCase, 'tags', name) ...
    || run_planCaseHasValue(planCase, 'resources', name);
end
%=============================================================================
function tf = run_planCaseHasValue(planCase, field, name)
  tf = false;
  if ~isfield(planCase, field)
    return
  end
  values = planCase.(field);
  if isempty(values)
    return
  end
  tf = any(strcmp(values, name));
end
%=============================================================================
