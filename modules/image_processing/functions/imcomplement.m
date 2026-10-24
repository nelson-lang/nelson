%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imcomplement(img)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if isa(img, 'logical')
    out = ~img;
  elseif isa(img, 'uint8')
    out = uint8(255) - img;
  elseif isa(img, 'uint16')
    out = uint16(65535) - img;
  elseif isa(img, 'int8')
    out = int8(-1 - double(img));
  elseif isa(img, 'int16')
    out = int16(-1 - double(img));
  else
    out = 1 - img;
  end
end
%=============================================================================
