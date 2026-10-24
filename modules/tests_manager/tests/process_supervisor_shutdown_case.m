%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
missing_executable = [nelsonroot(), '/missing-supervisor-executable'];
job = struct('executable', missing_executable, 'process_arguments', '', ...
  'launcher_timeout', 30);
__nelson_unittest_process_supervisor_start__(job, 1);
pause(1);
%=============================================================================
