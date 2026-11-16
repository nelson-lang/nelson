%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Start an ACP agent and perform the initialize and session/new handshake.
% Returns a client struct used by nelson.ai.acp.prompt and nelson.ai.acp.close.
function client = connect(command, varargin)
  narginchk(1, inf);
  timeoutSeconds = 30;
  proc = nelson.os.Process(command, varargin{:});
  if ~isvalid(proc)
    error('Nelson:ai:acpSpawnFailed', 'Unable to start the ACP agent.');
  end
  client = struct();
  client.proc = proc;
  initializeParams = struct('protocolVersion', 1, ...
    'clientInfo', struct('name', 'nelson', 'version', version()));
  client.initialize = acpSendRequest(proc, 1, 'initialize', initializeParams, timeoutSeconds);
  sessionParams = struct('cwd', pwd());
  % Expose Nelson's own MCP server to the agent so it gets the structured Nelson
  % tools and skills (evaluate_nelson_code, find_nelson_symbol, the nflow and
  % editor skills...). Gated behind NELSON_AI_SKILLS_CATALOG-style opt-in: the
  % server starts and connect() succeeds, but the agent currently stalls while
  % engaging the tools, so it stays OFF by default to keep the dock responsive.
  % Set NELSON_AI_ACP_MCP=1 to enable it (for debugging the tool round-trip).
  if isempty(getenv('NELSON_AI_ACP_MCP'))
    sessionParams.mcpServers = {};
  else
    sessionParams.mcpServers = nelsonMcpServers();
  end
  sessionResult = acpSendRequest(proc, 2, 'session/new', sessionParams, timeoutSeconds);
  if isfield(sessionResult, 'sessionId')
    client.sessionId = sessionResult.sessionId;
  else
    client.sessionId = '';
  end
  client.nextId = 3;
end
%=============================================================================
% Build the ACP mcpServers entry that launches Nelson's own MCP server (a
% separate adv-cli serving JSON-RPC over stdio, scoped to the current working
% directory). Returns an empty cell when the executable cannot be located, so the
% session is created without tools rather than failing.
function servers = nelsonMcpServers()
  servers = {};
  try
    exe = [modulepath('nelson', 'bin'), '/nelson-adv-cli'];
    if ispc()
      exe = [exe, '.exe'];
    end
    if ~isfile(exe)
      return
    end
    options = ['--workspace-root=', pwd(), ...
      ' --nelson-display-mode=adv-cli --max-output-characters=200000'];
    args = {'--quiet', '--no-ipc', '--no-user-startup', '--no-user-modules', '-e', ...
      ['nelson.ai.mcp.server(''', options, ''')']};
    servers = {struct('name', 'nelson', 'command', exe, 'args', {args}, 'env', {{}})};
  catch
    servers = {};
  end
end
%=============================================================================
