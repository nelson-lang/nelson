%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function C = bsxfun(f, A, B)
  % the function on the operands with implicit expansion, on the device; a function whose
  % result does not have the expanded size runs on the host
  done = false;
  if isa(f, 'function_handle')
    sa = size(A);
    sb = size(B);
    count = max(numel(sa), numel(sb));
    sa(end + 1:count) = 1;
    sb(end + 1:count) = 1;
    if all(sa == sb | sa == 1 | sb == 1)
      expected = max(sa, sb);
      expected(sa == 0 | sb == 0) = 0;
      C = f(A, B);
      sc = size(C);
      sc(end + 1:count) = 1;
      done = isequal(sc, expected);
    end
  end
  if ~done
    C = __gpuArray_host__('bsxfun', f, A, B);
  end
end
%=============================================================================
