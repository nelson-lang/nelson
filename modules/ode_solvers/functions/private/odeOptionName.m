%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = odeOptionName(name)
  if isstring(name)
    name = char(name);
  end
  if ~ischar(name)
    error('Nelson:ode_solvers:optionNameExpected', 'Option name expected.');
  end
  names = fieldnames(odeDefaultOptions());
  idx = find(strcmpi(name, names));
  if isempty(idx)
    matches = [];
    for k = 1:length(names)
      candidate = names{k};
      if length(name) <= length(candidate) && strcmpi(name, candidate(1:length(name)))
        matches(end + 1) = k;
      end
    end
    idx = matches;
  end
  if isempty(idx)
    error('Nelson:ode_solvers:unknownOdeOptionName', 'Unknown %s option name.', 'ODE');
  end
  if length(idx) > 1
    error('Nelson:ode_solvers:ambiguousOdeOptionName', 'Ambiguous %s option name.', 'ODE');
  end
  name = names{idx(1)};
end
%=============================================================================
