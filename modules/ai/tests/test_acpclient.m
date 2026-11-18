%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% <--NOT FIX FOR WASM-->
%=============================================================================
% Drive a mock ACP agent over the process object: initialize, session, prompt.
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-adv-cli'];
mock = [modulepath('ai', 'tests'), '/acp_mock_agent.m'];
client = nelson.ai.acp.internal.connect(nelson_exe, '--quiet', '--no-ipc', '--no-user-startup', ...
  '--no-user-modules', '-f', mock);
assert_isequal(client.initialize.agentInfo.name, 'mock-agent');
assert_isequal(client.sessionId, 'mock-session-1');
[answer, client] = nelson.ai.acp.internal.prompt(client, 'ping');
assert_isequal(answer, 'pong');
nelson.ai.acp.internal.close(client);
%=============================================================================
