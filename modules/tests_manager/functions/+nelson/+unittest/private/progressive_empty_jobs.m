%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function jobs = progressive_empty_jobs()
  jobs = struct('executables', {{}}, 'process_arguments', {{}}, ...
    'timeouts', [], 'kind', {{}}, 'index', [], 'metadata', {{}}, ...
    'job_id', {{}}, 'schedule_order', [], 'weight', [], 'reusable', [], ...
    'payload', {{}}, 'launcher', {{}}, 'result_file', {{}}, 'attempt', []);
end
%=============================================================================
