%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================

previous_live = getenv('NELSON_ENGINE_RUN_LIVE_TESTS');
setenv('NELSON_ENGINE_RUN_LIVE_TESTS', '0');

runner = [fileparts(nfilename('fullpathext')), '/run_nelson_engine_api_tests.py'];
status = pyrunfile(runner, 'status');

setenv('NELSON_ENGINE_RUN_LIVE_TESTS', previous_live);

status_value = double(status);
assert_isequal(status_value, 0);

%=============================================================================
