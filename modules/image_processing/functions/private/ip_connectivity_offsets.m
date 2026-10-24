%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function offsets = ip_connectivity_offsets(conn)
  if conn == 4
    offsets = [-1 0; 0 -1; 0 1; 1 0];
  elseif conn == 8
    offsets = [-1 -1; -1 0; -1 1; 0 -1; 0 1; 1 -1; 1 0; 1 1];
  else
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
end
%=============================================================================
