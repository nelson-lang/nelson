%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function z = realpow(x, y)
  narginchk(2, 2);
  nargoutchk(0, 1);

  if ~isreal(x) || ~isreal(y)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Inputs');
  end

  z = x .^ y;
  if ~isreal(z)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Result');
  end
end
%=============================================================================
