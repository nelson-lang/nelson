%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = bytecode_end_persistent_global_fun()
  % end used to index a global/persistent variable, including end nested in an
  % index expression (2:end-1, end-1:end). This used to raise
  % "END keyword illegal!" because such a reference was miscompiled as a call.
  global gEndPG
  gEndPG = [10 20 30 40];
  r.g_nested = gEndPG(2:end);
  r.g_direct = gEndPG(end);
  r.g_range = gEndPG(end - 1:end);

  persistent pEndPG
  pEndPG = reshape(1:12, 3, 4);
  r.p_read = pEndPG(2:end - 1, 3:end);
  % in-place assignment with end nested on both sides
  pEndPG(2:end - 1, 2:end - 1) = pEndPG(2:end - 1, 3:end);
  r.p_assigned = pEndPG;
  % loop with end-indexed in-place assignment on a persistent base
  pEndPG(end, :) = 0;
  for k = 1:2
    pEndPG(1:end - 1, 1) = pEndPG(2:end, 1);
  end
  r.p_looped = pEndPG;
end
%=============================================================================
