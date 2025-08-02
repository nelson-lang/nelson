%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = csvread(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  filename = varargin{1};
  mustBeTextScalar(filename, 1);
  
  filename = convertStringsToChars(filename);
  if ~isfile(filename) 
    error(_('File not found.'));
  end
  if (nargin < 2)
    R = 0;
  else
    R = varargin{2};
  end
  if (nargin < 3)
    C = 0;
  else
    C = varargin{3};
  end
  separator = ',';
  if (nargin < 4)
    M = dlmread(filename, separator, R, C); 
  else
    RNG = varargin{4};
    mustBeVector(RNG, 4);
    assert(numel(RNG) == 4, _('RNG must be a 4 elements vector.'));
    assert(RNG(1) == R, _('RNG(1) must be equal to R.'));
    assert(RNG(2) == C, _('RNG(2) must be equal to C.'));
    M = dlmread(filename, separator, RNG);
  end
  varargout{1} = M;
end
%=============================================================================
