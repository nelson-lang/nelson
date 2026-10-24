%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(obj)
  fprintf('  griddedInterpolant with properties:\n\n');
  fprintf('            GridVectors: %s\n', formatGridVectors(obj.GridVectors));
  fprintf('                 Values: %s\n', formatArray(obj.Values));
  fprintf('                 Method: ''%s''\n', obj.Method);
  fprintf('    ExtrapolationMethod: ''%s''\n', obj.ExtrapolationMethod);
end
%=============================================================================
function txt = formatGridVectors(gridVectors)
  if isempty(gridVectors)
    txt = '{}';
    return
  end
  parts = cell(1, numel(gridVectors));
  for k = 1:numel(gridVectors)
    parts{k} = formatArray(gridVectors{k});
  end
  txt = ['{', strjoin(parts, '  '), '}'];
end
%=============================================================================
function txt = formatArray(value)
  if isempty(value)
    txt = '[]';
    return
  end
  sz = size(value);
  if numel(sz) == 2 && min(sz) == 1 && max(sz) <= 10
    v = value(:).';
    items = cell(1, numel(v));
    for k = 1:numel(v)
      items{k} = num2str(v(k));
    end
    txt = ['[', strjoin(items, ' '), ']'];
  else
    dims = strjoin(arrayfun(@(d) num2str(d), sz, 'UniformOutput', false), 'x');
    txt = ['[', dims, ' ', class(value), ']'];
  end
end
%=============================================================================
