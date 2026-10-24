%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function x = hex2num(s, precision)
  narginchk(1, 2);
  if nargin < 2
    precision = 'double';
  end
  isSingle = strcmpi(precision, 'single');
  if isSingle
    nchars = 8;
    nbytes = 4;
  else
    nchars = 16;
    nbytes = 8;
  end
  if ischar(s) || isstring(s)
    rows = cellstr(s);
  elseif iscell(s)
    rows = s(:);
  else
    error('Nelson:elementary_functions:inputCharacterArrayString', 'Input must be a character array or string.');
  end
  x = zeros(numel(rows), 1);
  if isSingle
    x = single(x);
  end
  for r = 1:numel(rows)
    hexStr = char(rows{r});
    % Right-pad with zeros to a full-width representation.
    if length(hexStr) < nchars
      hexStr = [hexStr, repmat('0', 1, nchars - length(hexStr))];
    else
      hexStr = hexStr(1:nchars);
    end
    bytes = uint8(zeros(1, nbytes));
    for k = 1:nbytes
      bytes(k) = uint8(hex2dec(hexStr(2 * k - 1:2 * k)));
    end
    if isSingle
      x(r) = typecast(fliplr(bytes), 'single');
    else
      x(r) = typecast(fliplr(bytes), 'double');
    end
  end
end
%=============================================================================
