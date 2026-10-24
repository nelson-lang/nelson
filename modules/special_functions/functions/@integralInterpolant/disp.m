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
  fprintf('  integralInterpolant with properties:\n\n');
  fprintf('            Integrand: %s\n', func2str(obj.Integrand));
  fprintf('           LowerLimit: %s\n', formatValue(obj.LowerLimit));
  fprintf('           UpperLimit: %s\n', formatValue(obj.UpperLimit));
  fprintf('             Integral: %s\n', formatValue(obj.Integral));
  fprintf('           ErrorBound: %s\n', formatValue(obj.ErrorBound));
  fprintf('    AbsoluteTolerance: %s\n', formatValue(obj.AbsoluteTolerance));
  fprintf('    RelativeTolerance: %s\n', formatValue(obj.RelativeTolerance));
  fprintf('         Subintervals: %s\n', formatValue(obj.Subintervals));
end
%=============================================================================
function txt = formatValue(value)
  if isscalar(value)
    txt = num2str(value, 5);
    return
  end
  sz = size(value);
  if numel(sz) == 2 && min(sz) == 1 && max(sz) <= 20
    items = arrayfun(@(v) num2str(v, 5), value(:).', 'UniformOutput', false);
    txt = ['[', strjoin(items, ' '), ']'];
  else
    dims = strjoin(arrayfun(@(d) num2str(d), sz, 'UniformOutput', false), 'x');
    txt = ['[', dims, ' ', class(value), ']'];
  end
end
%=============================================================================
