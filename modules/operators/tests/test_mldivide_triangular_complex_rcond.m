%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Complex triangular RCOND measures each diagonal entry as |re| + |im|.
function test_mldivide_triangular_complex_rcond()
  % no warning: |re| + |im| ratio is above eps, the modulus ratio is not
  for c = [1.2e-16, 1.5e-16, 2e-16]
    checkAllForms(c, '', NaN);
  end
  for c = single([6e-8, 7e-8, 8e-8])
    checkAllForms(c, '', NaN);
  end
  checkAllForms(1e-16, 'Nelson:nearlySingularMatrix', 2e-16);
  checkAllForms(single(5e-8), 'Nelson:nearlySingularMatrix', 1e-7);
  % pure imaginary and real diagonals are unchanged
  checkAllForms(1i * 1e-16, 'Nelson:nearlySingularMatrix', 1e-16);
  checkAllForms(1i * 3e-16, '', NaN);
end
%=============================================================================
function checkAllForms(c, expectedId, expectedRcond)
  d = c * (1 + 1i);
  if ~isreal(c)
    d = c;
  end
  one = cast(1, 'like', real(d));
  T = [one, 0.5; 0, d];
  D = diag([one, d]);
  b = cast([1; 2], 'like', one);
  for k = 1:4
    lastwarn('', '');
    switch k
      case 1
        X = T \ b;
      case 2
        X = b.' / T;
      case 3
        X = D \ b;
      case 4
        X = b.' / D;
    end
    [msg, id] = lastwarn();
    asserts.isequal(id, expectedId);
    asserts.isequal(class(X), class(one));
    if ~isempty(expectedId)
      R = str2double(regexp(msg, 'RCOND = (\S+)\.$', 'tokens', 'once'));
      asserts.istrue(abs(R / expectedRcond - 1) < 1e-5);
    end
  end
end
%=============================================================================
