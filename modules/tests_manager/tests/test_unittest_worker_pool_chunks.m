%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
base = struct();
base.filename = '';
base.options = struct();
base.msg = '';
base.name = '';
base.classname = '';
base.status = '';
base.time = 0;
base.isbench = false;
base.skip = false;
base.exit_code = 0;
base.timed_out = false;
base.stdout = '';
base.stderr = '';
base.timeout = 0;
base.launcher_timeout = 0;
base.command = '';
base.executable = '';
base.process_arguments = '';
base.outputfile = '';
base.redirect_err = '';
base.command_filename = '';
base.runfile_result = [];
base.pool_index = 0;
base.pool_missing_result = false;
base.test_order_index = 0;
base.worker_pool_used = false;
base.worker_pool_fallback = false;
base.worker_pool_eligible = [];
base.worker_pool_native_jobs = 0;
base.worker_pool_requested_workers = 0;
base.worker_pool_inline_jobs = 0;
base.worker_pool_script_jobs = 0;
base.weight = 1;

cases = repmat(base, 1, 4);
caseWeights = [8, 4, 1, 1];
for k = 1:length(cases)
  cases(k).name = ['case_', int2str(k)];
  cases(k).weight = caseWeights(k);
end

chunks = worker_pool_chunks(cases, 2);
asserts.isequal(length(chunks), 2);

weights = zeros(1, 2);
for k = 1:2
  asserts.istrue(length(chunks{k}) > 0);
  asserts.istrue(all([chunks{k}.worker_pool_used]));
  weights(k) = sum([chunks{k}.weight]);
end

asserts.isequal(sort(weights), [6, 8]);
%=============================================================================
