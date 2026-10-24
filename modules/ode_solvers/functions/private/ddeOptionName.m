%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = ddeOptionName(name)
  if isstring(name)
    name = char(name);
  end
  if ~ischar(name)
    error('Nelson:ode_solvers:ddeOptionNameExpected', '%s option name expected.', 'DDE');
  end
  names = fieldnames(ddeDefaultOptions());
  idx = find(strcmpi(name, names));
  if isempty(idx)
    idx = ddePartialMatches(name, names);
  end
  if isempty(idx)
    error('Nelson:ode_solvers:unknownDdeOptionName', 'Unknown %s option name.', 'DDE');
  end
  if length(idx) > 1
    error('Nelson:ode_solvers:ambiguousDdeOptionName', 'Ambiguous %s option name.', 'DDE');
  end
  name = names{idx(1)};
end
%=============================================================================
function idx = ddePartialMatches(name, names)
  idx = [];
  for k = 1:length(names)
    candidate = names{k};
    if length(name) <= length(candidate) && strcmpi(name, candidate(1:length(name)))
      idx(end + 1) = k;
    end
  end
end
%=============================================================================
