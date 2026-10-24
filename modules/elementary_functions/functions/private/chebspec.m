%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function CMat = chebspec(nSize, kind, className)
  selectedKind = kind;
  if isempty(selectedKind)
    selectedKind = 0;
  end
  if selectedKind == 1
    effectiveSize = nSize + 1;
  else
    effectiveSize = nSize;
  end
  order = effectiveSize - 1;
  matrixSize = order + 1;
  CMat = zeros(matrixSize, className);
  nodes = cospi((0:order) / order).';
  scale = ones(matrixSize, 1, className);
  scale([1, matrixSize]) = 2;
  denominator = nodes - nodes.' + eye(matrixSize, className);
  CMat = scale(:, ones(1, matrixSize)) ./ denominator;
  signs = (-1) .^ (0:order);
  CMat = CMat .* (signs.' * signs);
  diagonal = zeros(matrixSize, 1, className);
  diagonal(1) = (2 * order ^ 2 + 1) / 6;
  diagonal(matrixSize) = -diagonal(1);
  if matrixSize > 2
    inner = 2:matrixSize - 1;
    diagonal(inner) = -nodes(inner) ./ (2 * (1 - nodes(inner) .^ 2));
  end
  CMat(1:matrixSize + 1:end) = diagonal;
  if selectedKind == 1
    CMat = CMat(2:matrixSize, 2:matrixSize);
  end
end
%=============================================================================
