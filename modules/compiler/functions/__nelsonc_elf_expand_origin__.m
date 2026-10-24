%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = __nelsonc_elf_expand_origin__(value, origin)
  for token = {'LIB', 'PLATFORM'}
    name = token{1};
    if contains(value, ['${', name, '}']) || contains(value, ['$', name, '/']) ...
      || endsWith(value, ['$', name])
      error('Nelson:nelsonc:nativeToken', 'Native loader token $%s requires target-platform resolution.', name);
    end
  end
  % Expand original tokens once; actual directory names may contain dollar signs.
  [starts, stops] = regexp(value, '\$(?:\{ORIGIN\}|ORIGIN(?=/|$))');
  if isempty(starts)
    return;
  end
  parts = cell(1, 2 * numel(starts) + 1);
  first = 1;
  for k = 1:numel(starts)
    parts{2 * k - 1} = value(first:starts(k) - 1);
    parts{2 * k} = origin;
    first = stops(k) + 1;
  end
  parts{end} = value(first:end);
  value = strjoin(parts, '');
end
