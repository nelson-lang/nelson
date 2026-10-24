%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = spfun(fun, S)
  narginchk(2, 2);
  if ~isa(fun, 'function_handle')
    error('Nelson:spfun:functionHandleExpected', 'First argument must be a function handle.');
  end
  if ~ismatrix(S)
    error('Nelson:sparse:ndSparseNotSupported', 'ND-sparse arrays are not supported.');
  end
  sparseInput = S;
  if ~issparse(S)
    sparseInput = sparse(S);
  end
  rowCount = size(sparseInput, 1);
  columnCount = size(sparseInput, 2);
  [rowIndex, columnIndex, nonzeroValues] = find(sparseInput); %#ok<NLS0004>
  if isempty(nonzeroValues)
    s = sparse([], [], [], rowCount, columnCount);
  else
    s = sparse(rowIndex, columnIndex, fun(nonzeroValues(:)), rowCount, columnCount);
  end
end
%=============================================================================
