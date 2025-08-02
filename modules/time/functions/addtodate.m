%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = addtodate (d, q, f)
  narginchk(3, 3);
  nargoutchk(0, 1);
  
  isSupportedThirdArgType = (ischar (f) && isrow (f)) || isStringScalar(f);
  if ~isSupportedThirdArgType
    error('Nelson:addtodate:InputDateField', _('Date field must be a character vector or string scalar.'));
  end
  f = convertStringsToChars(f);
  
  if (isscalar (d) && ~ isscalar (q))
    d = repmat (d, size (q));
  end
  
  switch f
    case 'year' 
      tmp = datevec (d);
      tmp(:, 1) = tmp(:, 1) + q(:);
      dnew = datenum (tmp);
      if (numel (d) == numel (dnew))
        d = reshape (dnew, size (d));
      else
        d = reshape (dnew, size (q));
      end
    case 'month'
      tmp = datevec (d);
      tmp(:, 2) = tmp(:, 2) + q(:);
      tmp(:, 1) = tmp(:, 1) + floor ((tmp(:, 2) - 1) / 12);
      tmp(:, 2) = mod (tmp(:, 2) - 1, 12) + 1;
      dnew = datenum (tmp);
      if (numel (d) == numel (dnew))
        d = reshape (dnew, size (d));
      else
        d = reshape (dnew, size (q));
      end        
    case 'day'
      d = d + q .* (1);
    case 'hour'
      d = d + q .* ( 1 / 24);       
    case 'minute'
      d = d + q .* (1 / 1440);       
    case 'second'
      d = d + q .* (1 / 86400);       
    case 'millisecond'
      d = d + q .* (1 / 86400000);
    otherwise
      msg = sprintf(_('Date field "%s" is invalid.'), f);
      error (msg);
    end
  end
  %=============================================================================
