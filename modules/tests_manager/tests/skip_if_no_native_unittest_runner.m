%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function skip_if_no_native_unittest_runner()
  hasProcessRunner = exist('__nelson_unittest_run_processes__', 'builtin') == 5;
  hasProcessSupervisor = ...
    exist('__nelson_unittest_process_supervisor_start__', 'builtin') == 5;
  skip_testsuite(~hasProcessRunner || ~hasProcessSupervisor, ...
    'Native unittest process supervisor is not available.');
end
