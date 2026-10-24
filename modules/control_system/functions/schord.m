%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function varargout = schord(Qi, Ti, index)
  arguments
    Qi {mustBeNumeric}
    Ti {mustBeNumeric}
    index {mustBeNumeric, mustBeVector}
  end
  nargoutchk(0, 2);

  rowCount = size(Ti, 1);
  columnCount = size(Ti, 2);
  if rowCount ~= columnCount
    error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 2, 'square matrix');
  end
  if ~isequal(size(Qi), size(Ti))
    error('Nelson:control_system:wrongSize:wrongSizeInputArguments12', 'Wrong size for input arguments #1 and #2: same size expected.');
  end
  if numel(index) ~= rowCount
    error('Nelson:control_system:wrongSize:wrongSizeArgument3VectorLength', 'Wrong size for argument #3: vector length must match matrix size.');
  end

  activeIndex = index(:).';
  To = Ti;
  Qo = Qi;

  for i = 1:(rowCount - 1)
    k = i;
    for j = (i + 1):rowCount
      if activeIndex(j) < activeIndex(k)
        k = j;
      end
    end
    for l = k:-1:(i + 1)
      l1 = l - 1;
      l2 = l;
      t = localGivens(To(l1, l1) - To(l2, l2), To(l1, l2));
      t = [t(2, :); t(1, :)];
      To(:, l1:l2) = To(:, l1:l2) * t;
      To(l1:l2, :) = t' * To(l1:l2, :);
      Qo(:, l1:l2) = Qo(:, l1:l2) * t;
      previousIndex = activeIndex(l1);
      activeIndex(l1) = activeIndex(l2);
      activeIndex(l2) = previousIndex;
    end
  end

  varargout{1} = Qo;
  if nargout > 1
    varargout{2} = To;
  end
end
%=============================================================================
function G = localGivens(a, b)
  if b == 0
    G = eye(2, 2);
    return
  end
  r = hypot(abs(a), abs(b));
  c = a / r;
  s = b / r;
  G = [c, s; -conj(s), conj(c)];
end
%=============================================================================
