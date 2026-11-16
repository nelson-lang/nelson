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
% The global permission mode gates the file-access prompt: 'always_ask' prompts,
% 'auto' prompts only for risky requests, 'full' never prompts. Check the mode
% store, the risk classification, and that 'full' auto-allows a request end to end.
% The mode now persists to prefdir, so back up the real file, start from a clean
% slate, and restore it at the end so the suite never overwrites the user choice.
prefFile = [prefdir(), '/assistant/permission_mode.json'];
hadPref = isfile(prefFile);
prefBackup = '';
if hadPref
  prefBackup = fileread(prefFile);
  [~] = rmfile(prefFile);
end
ok = true;
errMsg = '';
try
  asserts.isequal(nelson.ai.acp.permissionMode(), 'always_ask');
  asserts.isequal(nelson.ai.acp.permissionMode('auto'), 'auto');
  asserts.isequal(nelson.ai.acp.permissionMode(), 'auto');
  % An unrecognized value is ignored (stays 'auto').
  asserts.isequal(nelson.ai.acp.permissionMode('bogus'), 'auto');
  asserts.isequal(nelson.ai.acp.permissionMode('full'), 'full');
  % Risk classification (private helper, reached by putting its folder on path).
  addpath([modulepath('ai', 'functions'), '/+nelson/+ai/+acp/private']);
  asserts.istrue(~acpIsRisky(struct('tool', 'read file', 'kind', 'read')));
  asserts.istrue(acpIsRisky(struct('tool', 'write file', 'kind', 'write')));
  asserts.istrue(acpIsRisky(struct('tool', 'mystery', 'kind', '')));
  % Mode 'full': the agent's permission request is auto-allowed, so the run
  % completes WITHOUT any manual reply.
  nelson.ai.acp.permissionMode('full');
  binpath = modulepath('nelson', 'bin');
  nelson_exe = [binpath, '/nelson-adv-cli'];
  mock = [modulepath('ai', 'tests'), '/acp_mock_agent_permission.m'];
  client = nelson.ai.acp.connect(nelson_exe, '--quiet', '--no-ipc', '--no-user-startup', ...
    '--no-user-modules', '-f', mock);
  client = nelson.ai.acp.send(client, 'edit a file');
  done = false;
  text = '';
  for k = 1:500
    [done, text, client] = nelson.ai.acp.poll(client, []);
    if done
      break
    end
    pause(0.02);
  end
  asserts.istrue(done);
  asserts.istrue(contains(text, 'permission-answered'));
  nelson.ai.acp.close(client);
  nelson.ai.acp.permissionMode('always_ask');
  asserts.isequal(nelson.ai.acp.permissionMode(), 'always_ask');
catch e
  ok = false;
  errMsg = e.message;
end
% Restore the user's real preference whatever the outcome above.
if isfile(prefFile)
  [~] = rmfile(prefFile);
end
if hadPref
  filewrite(prefFile, prefBackup);
end
if ~ok
  error(errMsg);
end
%=============================================================================
