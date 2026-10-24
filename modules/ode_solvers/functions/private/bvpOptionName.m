%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = bvpOptionName(name)
  if isstring(name)
    name = char(name);
  end
  if ~ischar(name)
    error('Nelson:ode_solvers:bvpOptionNameExpected', '%s option name expected.', 'BVP');
  end
  names = fieldnames(bvpDefaultOptions());
  idx = find(strcmpi(name, names));
  if isempty(idx)
    idx = bvpPartialMatches(name, names);
  end
  if isempty(idx)
    error('Nelson:ode_solvers:unknownBvpOptionName', 'Unknown %s option name.', 'BVP');
  end
  if length(idx) > 1
    error('Nelson:ode_solvers:ambiguousBvpOptionName', 'Ambiguous %s option name.', 'BVP');
  end
  name = names{idx(1)};
end
%=============================================================================
function idx = bvpPartialMatches(name, names)
  idx = [];
  for k = 1:length(names)
    candidate = names{k};
    if length(name) <= length(candidate) && strcmpi(name, candidate(1:length(name)))
      idx(end + 1) = k;
    end
  end
end
%=============================================================================
