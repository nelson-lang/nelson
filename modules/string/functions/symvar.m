%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = symvar(s)
  narginchk(1, 1);
  if isa(s, 'function_handle')
    s = func2str(s);
  end
  if isstring(s)
    if ~isscalar(s)
      error('Nelson:string:inputArgumentCharacterVectorString', 'Input argument must be a character vector or a string scalar.');
    end
    s = char(s);
  end
  if ~ischar(s) || (~isempty(s) && ~isrow(s))
    error('Nelson:string:inputArgumentCharacterVectorString', 'Input argument must be a character vector or a string scalar.');
  end
  % Identifiers, together with the character that follows them (after optional
  % spaces), so that a name written as a function call, name(...), can be
  % detected. A name preceded by '.' is a field access and is not matched.
  tokens = regexp(s, '(?<![A-Za-z0-9_.])([A-Za-z]\w*)\s*(\()?', 'tokens');
  % Special values are never reported as variables.
  special = {'i', 'j', 'pi', 'inf', 'Inf', 'nan', 'NaN', 'eps'};
  names = cell(0, 1);
  for k = 1:numel(tokens)
    token = tokens{k};
    name = token{1};
    isCall = (numel(token) >= 2) && ~isempty(token{2});
    if isCall
      continue;
    end
    if any(strcmp(name, special))
      continue;
    end
    names{end + 1, 1} = name; %#ok<AGROW>
  end
  if isempty(names)
    out = cell(0, 1);
  else
    out = unique(names);
  end
end
%=============================================================================
