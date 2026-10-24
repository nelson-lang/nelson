%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 1000000;
R = 9;
for mode = 1:3
  samples = zeros(1, R);
  for repetition = 1:R
    if mode == 1
      a = 0;
      b = 1;
    elseif mode == 2
      a = single(0);
      b = single(1);
    else
      flag = false;
    end
    tic;
    if mode <= 2
      for k = 1:N
        c = a + b;
        a = b;
        b = c;
      end
    else
      for k = 1:N
        flag = ~flag;
      end
    end
    samples(repetition) = toc;
    if mode <= 2
      asserts.isequal(double([a, b, c]), [Inf, Inf, Inf]);
      if mode == 2
        asserts.isequal(class(b), 'single');
      end
    else
      asserts.isequal(flag, false);
    end
  end
  fprintf('Scalar loop mode %d: %.9f s (median)\n', mode, median(samples));
end
