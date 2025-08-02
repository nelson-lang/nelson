%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = array2table(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  X = varargin{1};
  if ~ismatrix(X)
    error(_('X must be a 2-D matrix.'));
  end
  nCols = size (X, 2);
  values = cell(1, nCols);
  for iCol = 1:nCols
    values{iCol} = X(:, iCol);
  end
  prefix = inputname(1);
  if isempty(prefix)
    prefix = 'Var';
  end
  varNames = cell (1, nCols);
  for iCol = 1:nCols
    varNames{iCol} = sprintf('%s%d', prefix, iCol);
  end
  varargout{1} = table(values{:}, 'VariableNames', varNames); 
end
%=============================================================================
