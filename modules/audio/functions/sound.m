%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sound(varargin)
  narginchk(1, 3);
  y = varargin{1};
  if (~isreal(y) || issparse(y) || ~isfloat(y))
    error(_('Audio data must be real and floating point.'));
  else
    y = double(y);
  end
  if nargin > 1
    fs = varargin{2};
  else
    fs = 8192;
  end
  if nargin > 2
    nbits = varargin{3};
  else 
    nbits = 16;
  end
  if (~(isscalar (fs) && (fs > 0)))
    error (_('Sample rate FS must be a positive number.'));
  end
  isSupportedBits = isscalar(nbits) && ((nbits == 8) || (nbits == 16) || (nbits == 24));
  if (~isSupportedBits)
    error (_('NBITS must be 8, 16, or 24.'));
  end
  if (isempty(y))
    return;
  end
  y = max(-1, min(y, 1));
  if ndims(y) > 2
    error(_('Argument #1: Requires 2-D values only.'));
  end
  if (size(y, 1) == 1)
    y = y .';
  end
  play = audioplayer(y, fs, nbits);
  playblocking (play);
  delete(play);
end
