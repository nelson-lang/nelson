%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mag2db(varargin)
  narginchk (1, 1);
  nargoutchk (0, 1);
  mag = varargin{1};
  db = 20 .* log10 (mag);
  db(mag < 0) = NaN;
  if all(imag(db) == 0)
    db = real(db);
  end
  varargout{1} = db; 
end
%=============================================================================
