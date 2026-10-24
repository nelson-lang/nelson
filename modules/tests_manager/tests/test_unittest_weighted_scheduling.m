%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_if_no_native_unittest_runner();
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelsonExe = [binpath, '/nelson-cli'];
if ispc()
  nelsonExe = [nelsonExe, '.exe'];
end
jobs = run_process_jobs({nelsonExe; nelsonExe; nelsonExe}, ...
  {'--quiet --no-ipc --no-user-modules -e "disp(''light'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "disp(''heavy'')"'; ...
  '--quiet --no-ipc --no-user-modules -e "disp(''medium'')"'}, ...
  [20, 20, 20], {'test'; 'test'; 'test'}, [1, 2, 3], ...
  {'light'; 'heavy'; 'medium'}, {'light'; 'heavy'; 'medium'});
jobs.weight = [1, 8, 4];
jobs.schedule_order = [1, 2, 3];
events = run_processes_progressive(jobs, 1);
processOrder = {};
for k = 1:length(events)
  if strcmp(events{k}.event_type, 'process')
    processOrder{end + 1} = events{k}.job_id; %#ok<AGROW>
  end
end
asserts.isequal(processOrder, {'heavy', 'medium', 'light'});
%=============================================================================
