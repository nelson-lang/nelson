%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytes = bytesFromData(data)
  % uint8 data as a row vector; char or string scalar as UTF-8 bytes.
  if isa(data, 'uint8')
    bytes = reshape(data, 1, []);
  elseif ischar(data) || (isstring(data) && isscalar(data))
    bytes = unicode2native(char(data), 'UTF-8');
  else
    error('Nelson:io:invalidData', 'Data must be uint8, char or a string scalar.');
  end
end
%=============================================================================
