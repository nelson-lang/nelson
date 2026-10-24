%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function units = tableNormalizedUnits(props)
  % The VariableUnits of props as one char unit per variable ('' if unset).
  units = repmat({''}, 1, length(props.VariableNames));
  if ~isfield(props, 'VariableUnits') || isempty(props.VariableUnits)
    return
  end
  raw = props.VariableUnits;
  if isstring(raw)
    raw = cellstr(raw);
  end
  for k = 1:min(length(raw), length(units))
    value = raw{k};
    if isstring(value)
      value = char(value);
    end
    if isempty(value)
      units{k} = '';
    else
      units{k} = char(value);
    end
  end
end
%=============================================================================
