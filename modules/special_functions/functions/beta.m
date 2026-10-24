%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = beta(a, b)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if ~isreal(a) || ~isreal(b)
    error('Nelson:beta:mustBeReal', 'Inputs must be real.');
  end
  y = exp(betaln(a, b));
  s = sign(gamma(a)) .* sign(gamma(b)) .* sign(gamma(a + b));
  y = y .* s;
end
%=============================================================================
