%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [cleanupRegistry, entry] = run_registerRun(plan, outputFile)
  registryFile = [unittest_tempdir(), 'nelson_unittest_registry_', createGUID(), '.json'];
  entry = struct('created_at', run_timestamp(), 'registry_file', registryFile, ...
    'output_file', outputFile, 'runner_pid', run_runnerPid(), 'workers', plan.workers, ...
    'count', length(plan.cases), 'cases', {run_caseIds(plan)}, 'state', 'running', ...
    'process_policy', plan.resource_policy);
  filewrite(registryFile, jsonprettyprint(jsonencode(entry)));
  cleanupRegistry = onCleanup(@() run_removeFile(registryFile));
end
