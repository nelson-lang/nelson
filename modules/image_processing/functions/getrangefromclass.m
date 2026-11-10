%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function range = getrangefromclass(I)
  % display range of an image class: [0 1] for floating and logical images, the full span for integers
  narginchk(1, 1);
  t = underlyingType(I);
  switch t
    case {'double', 'single', 'logical'}
      range = [0, 1];
    case {'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64'}
      range = double([intmin(t), intmax(t)]);
    otherwise
      error('Nelson:getrangefromclass:invalidType', 'I must be a numeric or logical array.');
  end
end
%=============================================================================
