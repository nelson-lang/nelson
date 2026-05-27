%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function idx = tableResolveVariables(names, vars, types, data)
  if nargin < 3
    types = [];
  end
  if nargin < 4
    data = struct();
  end
  if nargin < 2 || isempty(vars) || (ischar(vars) && strcmp(vars, ':'))
    idx = 1:length(names);
    return
  end
  if strcmp(class(vars), 'vartype')
    idx = find(strcmp(types, vars.TypeName));
    return
  end
  if isa(vars, 'function_handle')
    idx = [];
    for k = 1:length(names)
      try
        tf = vars(types(k));
      catch
        tf = vars(data.(names{k}));
      end
      if tf
        idx(end + 1) = k;
      end
    end
    return
  end
  if islogical(vars)
    idx = find(vars);
    return
  end
  if isnumeric(vars)
    idx = vars;
    return
  end
  if isstring(vars)
    vars = cellstr(vars);
  elseif ischar(vars)
    vars = {vars};
  end
  idx = zeros(1, length(vars));
  for k = 1:length(vars)
    found = find(strcmp(names, vars{k}), 1);
    if isempty(found)
      error(_('Unrecognized table variable name.'));
    end
    idx(k) = found;
  end
end
%=============================================================================
