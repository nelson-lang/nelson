%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Classify a pending permission for the 'auto' mode: safe requests (reading,
% listing, searching) are auto-allowed; anything that writes, executes or touches
% the network is risky and still prompts. Best-effort on the tool/kind text, and
% UNKNOWN counts as risky so 'auto' never silently allows something unrecognized.
function tf = acpIsRisky(perm)
  narginchk(1, 1);
  nargoutchk(0, 1);
  hay = lower([char(perm.tool), ' ', char(perm.kind)]);
  risky = {'write', 'edit', 'modify', 'create', 'delete', 'remove', 'move', ...
    'rename', 'execute', 'exec', 'run', 'fetch', 'http', 'network', 'download', ...
    'upload', 'install', 'kill'};
  for k = 1:numel(risky)
    if contains(hay, risky{k})
      tf = true;
      return
    end
  end
  safe = {'read', 'view', 'list', 'search', 'show', 'open', 'fetch_nelson_help'};
  for k = 1:numel(safe)
    if contains(hay, safe{k})
      tf = false;
      return
    end
  end
  tf = true;
end
%=============================================================================
