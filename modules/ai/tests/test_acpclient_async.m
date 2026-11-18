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
% Drive the mock ACP agent through the NON-BLOCKING send/poll path (the one the
% desktop panes and the Qt dock use): send without waiting, then poll until the
% reply arrives, checking the pending state is set then cleared.
binpath = modulepath('nelson', 'bin');
nelson_exe = [binpath, '/nelson-adv-cli'];
mock = [modulepath('ai', 'tests'), '/acp_mock_agent.m'];
client = nelson.ai.acp.internal.connect(nelson_exe, '--quiet', '--no-ipc', '--no-user-startup', ...
  '--no-user-modules', '-f', mock);
asserts.isequal(client.sessionId, 'mock-session-1');
%=============================================================================
% send returns immediately and marks a pending request active.
client = nelson.ai.acp.internal.send(client, 'ping');
asserts.istrue(client.pending.active);
%=============================================================================
% poll reads incrementally; the reply ('pong') arrives and done becomes true.
done = false;
text = '';
for k = 1:300
  [done, text, client] = nelson.ai.acp.internal.poll(client, []);
  if done
    break
  end
  pause(0.02);
end
asserts.istrue(done);
asserts.isequal(strtrim(text), 'pong');
%=============================================================================
% the pending request is cleared once the reply is complete.
asserts.istrue(~client.pending.active);
%=============================================================================
nelson.ai.acp.internal.close(client);
%=============================================================================
