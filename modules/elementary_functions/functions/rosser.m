%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = rosser(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  
  classname = 'double';
  if nargin == 1
    classname = varargin{1};
  end
  if isStringScalar(classname)
    classname = convertStringsToChars(classname);
  end
  
  R  = [611,  196, -192,  407,   -8,  -52,  -49,   29;
  196,  899,  113, -192,  -71,  -43,   -8,  -44;
  -192,  113,  899,  196,   61,   49,    8,   52;
  407, -192,  196,  611,    8,   44,   59,  -23;
  -8,  -71,   61,    8,  411, -599,  208,  208;
  -52,  -43,   49,   44, -599,  411,  208,  208;
  -49,   -8,    8,   59,  208,  208,   99, -911;
  29,  -44,   52,  -23,  208,  208, -911,   99];
  
  R = cast(R, classname);
end