%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [trans, emis, nstates, nsymbols] = __hmm_check_model(trans, emis)
  trans = checkMatrix(trans, sprintf('%s must be a finite nonnegative numeric matrix.', 'TRANS'));
  emis = checkMatrix(emis, sprintf('%s must be a finite nonnegative numeric matrix.', 'EMIS'));
  if size(trans, 1) ~= size(trans, 2)
    error('Nelson:statistics:TRANSSquareMatrix', '%s must be a square matrix.', 'TRANS');
  end
  if size(emis, 1) ~= size(trans, 1)
    error('Nelson:statistics:TRANSEMISSameNumberStates', 'TRANS and EMIS must have the same number of states.');
  end
  trans = normalizeRows(trans, sprintf('%s must have positive sum.', 'Each TRANS row'));
  emis = normalizeRows(emis, sprintf('%s must have positive sum.', 'Each EMIS row'));
  nstates = size(trans, 1);
  nsymbols = size(emis, 2);
end
%=============================================================================
function A = checkMatrix(A, message)
  if ~(isnumeric(A) && isreal(A) && ~issparse(A) && ndims(A) == 2)
    error('Nelson:statistics:realNumericMatrix', message);
  end
  A = double(A);
  if isempty(A) || any(~isfinite(A(:))) || any(A(:) < 0)
    error('Nelson:statistics:invalidValue', message);
  end
end
%=============================================================================
function A = normalizeRows(A, message)
  rowsum = sum(A, 2);
  if any(rowsum <= 0)
    error('Nelson:statistics:invalidValue', message);
  end
  A = A ./ repmat(rowsum, 1, size(A, 2));
end
%=============================================================================
