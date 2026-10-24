%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = tensorprod(A, B, varargin)
  narginchk(2, inf);
  if ~isnumeric(A) || ~isnumeric(B)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Inputs');
  end
  [dimA, dimB, contractAll, numDimsA] = parseArguments(varargin);
  ndA = ndims(A);
  if ~isempty(numDimsA)
    ndA = numDimsA;
  end
  ndB = ndims(B);
  if contractAll
    dimA = 1:ndA;
    dimB = 1:ndB;
  else
    if ~isempty(dimA)
      ndA = max(ndA, max(dimA));
    end
    if ~isempty(dimB)
      ndB = max(ndB, max(dimB));
    end
  end
  if numel(dimA) ~= numel(dimB)
    error('Nelson:tensorprod:contractedDimensionsMismatch', 'The number of contracted dimensions must match for both arrays.');
  end
  szA = sizePadded(A, ndA);
  szB = sizePadded(B, ndB);
  for k = 1:numel(dimA)
    if szA(dimA(k)) ~= szB(dimB(k))
      error('Nelson:dimagree:specifiedDimensionsInputArraysSizes', 'Specified dimensions of the input arrays must have the same sizes.');
    end
  end
  remA = setdiff(1:ndA, dimA, 'stable');
  remB = setdiff(1:ndB, dimB, 'stable');
  M1 = reshape(permute(A, [remA, dimA]), prod(szA(remA)), prod(szA(dimA)));
  M2 = reshape(permute(B, [dimB, remB]), prod(szB(dimB)), prod(szB(remB)));
  P = M1 * M2;
  outSize = [szA(remA), szB(remB)];
  if isempty(outSize)
    outSize = [1 1];
  elseif isscalar(outSize)
    outSize = [outSize, 1];
  end
  C = reshape(P, outSize);
end
%=============================================================================
function [dimA, dimB, contractAll, numDimsA] = parseArguments(args)
  dimA = [];
  dimB = [];
  contractAll = false;
  numDimsA = [];
  positional = {};
  k = 1;
  while k <= numel(args)
    a = args{k};
    if (ischar(a) || (isstring(a) && isscalar(a))) && strcmpi(char(a), 'NumDimensionsA')
      if k + 1 > numel(args)
        error('Nelson:tensorprod:valueExpected', 'Value expected after ''NumDimensionsA''.');
      end
      numDimsA = double(args{k + 1});
      k = k + 2;
    elseif (ischar(a) || (isstring(a) && isscalar(a))) && strcmpi(char(a), 'all')
      contractAll = true;
      k = k + 1;
    else
      positional{end + 1} = a;
      k = k + 1;
    end
  end
  if numel(positional) == 2
    dimA = double(positional{1});
    dimA = dimA(:).';
    dimB = double(positional{2});
    dimB = dimB(:).';
  elseif ~isempty(positional)
    error('Nelson:tensorprod:invalidContractedDimensions', 'Contracted dimensions must be given as two vectors DIMA and DIMB.');
  end
end
%=============================================================================
function sz = sizePadded(X, nd)
  sz = size(X);
  if numel(sz) < nd
    sz = [sz, ones(1, nd - numel(sz))];
  end
end
%=============================================================================
