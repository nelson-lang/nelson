%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tfdata(varargin)
  % Access state-space model data
  % [a, b, c, d] = ssdata(sys)
  % [a, b, c, d, Ts] = ssdata(sys)
  narginchk(1, 1);
  nargoutchk(0, 3);
  sys = varargin{1};
  
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  sys = tf(sys);
  numerator = sys.Numerator;
  denominator = sys.Denominator;
  Ts = sys.Ts;
  if isscalar(numerator)
    numerator = numerator{1};
    denominator = denominator{1};
  end
  varargout{1} = numerator;
  if nargout > 1
    varargout{2} = denominator;
  end
  if nargout > 2
    varargout{3} = Ts;
  end
end
