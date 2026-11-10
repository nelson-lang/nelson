%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function E = expm(A)
  % matrix exponential on the device: scaling and squaring with the degree 13 Pade approximant
  if ~(isfloat(A) && isreal(A) && ismatrix(A) && size(A, 1) == size(A, 2) && ~isempty(A))
    E = __gpuArray_host__('expm', A);
    return
  end
  n = size(A, 1);
  normA = double(gather(max(sum(abs(A), 1))));
  if ~isfinite(normA)
    E = __gpuArray_host__('expm', A);
    return
  end
  theta = 5.371920351148152;
  s = max(0, ceil(log2(normA / theta)));
  A = A / 2 ^ s;
  b = [64764752532480000, 32382376266240000, 7771770303897600, 1187353796428800, ...
       129060195264000, 10559470521600, 670442572800, 33522128640, 1323241920, ...
       40840800, 960960, 16380, 182, 1];
  I = eye(n, 'like', A);
  A2 = A * A;
  A4 = A2 * A2;
  A6 = A2 * A4;
  U = A * (A6 * (b(14) * A6 + b(12) * A4 + b(10) * A2) + b(8) * A6 + b(6) * A4 + b(4) * A2 + b(2) * I);
  V = A6 * (b(13) * A6 + b(11) * A4 + b(9) * A2) + b(7) * A6 + b(5) * A4 + b(3) * A2 + b(1) * I;
  E = (V - U) \ (V + U);
  for k = 1:s
    E = E * E;
  end
end
%=============================================================================
