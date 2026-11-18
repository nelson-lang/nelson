%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The stored conversation of one surface as a JSON array string, ready for a host
% pane to parse and re-display on open (so a restart restores the prior turns).
% Always an array '[{"role":...,"text":...},...]', or '[]' when there is no
% history: the turns are wrapped in a cell so a single turn still encodes as an
% array, not a bare object, and so empty encodes as '[]'.
function txt = json(surfaceId)
  narginchk(1, 1);
  nargoutchk(0, 1);
  turns = nelson.ai.internal.history.load(surfaceId);
  items = cell(1, numel(turns));
  for k = 1:numel(turns)
    items{k} = struct('role', turns(k).role, 'text', turns(k).text);
  end
  txt = jsonencode(items);
end
%=============================================================================
