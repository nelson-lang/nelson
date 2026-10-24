%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function worker = worker_pool_create_worker(chunk, privateDir, runtimeOptions)
  worker = worker_pool_empty_worker();
  worker.executable = getNelsonExecutablePath('nelson-cli');
  cmd = test_case_process_options(chunk(1));
  userArguments = test_case_process_user_arguments(chunk(1));
  worker.launcher_timeout = worker_pool_timeout(chunk);
  code = worker_pool_execute_code(chunk, privateDir);
  if worker_pool_should_inline(runtimeOptions, code, cmd)
    worker.process_arguments = [worker_pool_base_arguments(cmd, worker.launcher_timeout), ...
      ' --execute ', quoteProcessArgument(code)];
  else
    worker.command_filename = worker_pool_write_script(chunk, privateDir);
    worker.process_arguments = [worker_pool_base_arguments(cmd, worker.launcher_timeout), ...
      ' --file ', quoteProcessArgument(worker.command_filename)];
  end
  worker.process_arguments = append_process_user_arguments(worker.process_arguments, userArguments);
end
%=============================================================================
