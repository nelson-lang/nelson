%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Load the stored conversation of one surface as a struct array of turns with
% fields role ('user'|'assistant') and text. Returns an empty struct array when
% there is no history or the file is missing/corrupt, so a bad file never breaks
% the assistant.
function turns = load(surfaceId)
  narginchk(1, 1);
  nargoutchk(0, 1);
  turns = emptyTurns();
  f = historyFile(surfaceId);
  if ~isfile(f)
    return
  end
  try
    data = jsondecode(f, '-file');
  catch
    return
  end
  turns = normalizeTurns(data);
end
%=============================================================================
function turns = emptyTurns()
  turns = struct('role', {}, 'text', {});
end
%=============================================================================
% Coerce whatever jsondecode returned (a scalar struct for one turn, a struct
% array for many, or [] for none) into a clean struct array with char fields,
% dropping any malformed entry.
function turns = normalizeTurns(data)
  turns = emptyTurns();
  if ~isstruct(data)
    return
  end
  for k = 1:numel(data)
    item = data(k);
    if ~isfield(item, 'role') || ~isfield(item, 'text')
      continue
    end
    role = char(item.role);
    if ~any(strcmp(role, {'user', 'assistant'}))
      continue
    end
    turns(end + 1) = struct('role', role, 'text', char(item.text)); %#ok<AGROW>
  end
end
%=============================================================================
