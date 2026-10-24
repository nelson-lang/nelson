%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [value, found] = jsonPointerGet(value, pointer)
  % Resolves a JSON Pointer (text or token list) on a decoded JSON value.
  % Returns [] and found = false when the node does not exist.
  if iscell(pointer)
    tokens = pointer;
  else
    tokens = jsonPointerTokens(pointer);
  end
  found = true;
  for k = 1:numel(tokens)
    token = tokens{k};
    if isstruct(value) && isscalar(value)
      name = jsonFieldName(value, token);
      if isempty(name)
        value = [];
        found = false;
        return
      end
      value = value.(name);
    elseif ~isempty(regexp(token, '^\d+$', 'once')) && str2double(token) < numel(value)
      index = str2double(token) + 1;
      if iscell(value)
        value = value{index};
      else
        value = value(index);
      end
    else
      value = [];
      found = false;
      return
    end
  end
end
%=============================================================================
