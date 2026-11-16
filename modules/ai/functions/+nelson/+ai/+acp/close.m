%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Close a connected ACP agent and release its process.
function close(client)
  narginchk(1, 1);
  if isfield(client, 'proc') && isvalid(client.proc)
    delete(client.proc);
  end
end
%=============================================================================
