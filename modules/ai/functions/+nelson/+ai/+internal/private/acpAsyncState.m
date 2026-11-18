%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Process-persistent ACP clients, keyed by surfaceId ('web' / 'qt' / 'nflow'), so
% each assistant surface owns an ISOLATED session: a prompt on one surface no
% longer blocks or leaks into another. Shared between startAcpAsync (opens the
% request) and the poll steps (Command Window timer or Qt dock QTimer).
function out = acpAsyncState(op, surfaceId, value)
  narginchk(2, 3);
  persistent clients
  if isempty(clients)
    clients = struct();
  end
  out = [];
  key = char(surfaceId);
  switch op
    case 'get'
      if isfield(clients, key)
        out = clients.(key);
      end
    case 'set'
      clients.(key) = value;
    case 'reset'
      if isfield(clients, key)
        clients = rmfield(clients, key);
      end
    case 'surfaces'
      out = fieldnames(clients);
  end
end
%=============================================================================
