%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('advanced CLI launcher is not available.');
% web figures have no widget: the scenario runs in a child started with --webview
nelson_adv_cli = [modulepath('nelson', 'bin'), '/nelson-adv-cli'];
cli_available = isfile(nelson_adv_cli) || (ispc() && isfile([nelson_adv_cli, '.exe']));
skip_testsuite(~cli_available, ['Executable not found: ', nelson_adv_cli]);
scenario = [fileparts(mfilename('fullpathext')), '/web_size_changed_fcn_scenario.m'];
asserts.istrue(isfile(scenario));
command = ['"', nelson_adv_cli, '" --webview --quiet --no-user-startup --no-ipc -f "', scenario, '"'];
result = [tempname(), '.txt'];
setenv('NELSON_WEB_SIZE_CHANGED_FCN_RESULT', result);
% a child that died before the scenario writes no result; one that stopped
% later leaves the name of the check it was running
[status, output] = system(command, 120);
setenv('NELSON_WEB_SIZE_CHANGED_FCN_RESULT', '');
asserts.istrue(isfile(result), output);
verdict = fileread(result);
delete(result);
asserts.isequal(verdict, 'OK');
%=============================================================================
