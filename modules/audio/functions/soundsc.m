%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function soundsc(varargin)
  narginchk(1, 4);
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
  if nargin > 3
    yrange = varargin{4};
  else
    yrange = [-max(abs(y)), max(abs(y))];
  end
  
  if (~(isscalar (fs) && (fs > 0)))
    error (_('Sample rate FS must be a positive number.'));
  end
  isSupportedBits = isscalar(nbits) && ((nbits == 8) || (nbits == 16) || (nbits == 24));
  if (~isSupportedBits)
    error (_('NBITS must be 8, 16, or 24.'));
  end
  if (isempty (yrange))
    yrange = [-max(abs(y)), max(abs(y))] ;
  end 
  isMinMaxVector = isreal(yrange) && (numel(yrange) == 2) && (yrange(1) <= yrange(2));
  if ~isMinMaxVector
    error (_('RANGE must be a 2-element [YMIN, YMAX] vector.'));        
  end
  ymin = double(yrange(1));
  ymax = double(yrange(2));
  ymedian = (ymax + ymin) / 2;
  yscale = 2 / (ymax - ymin);
  y = (y - ymedian) .* yscale;
  sound(y, fs, nbits);
end
