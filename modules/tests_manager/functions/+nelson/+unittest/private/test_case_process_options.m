%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function cmd = test_case_process_options(test_case)
  cmd = '';
  cmd = append_process_option(cmd, test_case.options.english_imposed, '--language en_US');
  cmd = append_process_option(cmd, true, '--no-user-modules');
  cmd = append_process_option(cmd, ~test_case.options.ipc_required, '--no-ipc');
  cmd = append_process_option(cmd, ~test_case.options.file_watcher_required, '--without-file-watcher');
end
%=============================================================================
