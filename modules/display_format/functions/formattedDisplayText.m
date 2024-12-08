%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = formattedDisplayText(varargin)
  nbArgsValid = mod(nargin, 2) == 1;
  if ~nbArgsValid
    error(_('Wrong number of input arguments.'));
  end
  currentFormat = format();
  usedFormat = setFormatWithArguments(currentFormat, varargin);
  variable = varargin{1};
  result = formatVariable(variable, usedFormat);
  format(currentFormat);
end
%=============================================================================
function newFormat = setFormatWithArguments(currentFormat, args)
  newFormat = currentFormat;
  for i = 2 : 2 : length(args) - 1
    validField = false;
    name = lower(args{i});
    value = args{i + 1};
    switch name
      case 'numericformat'
      validateNumericFormat(value, i + 1);
      newFormat.NumericFormat = value;
      validField = true;

      case 'linespacing'
      validateLineSpacing(value, i + 1);
      newFormat.LineSpacing = value;
      validField = true;

      case {'suppressmarkup', 'usetruefalseforlogical'}
      % not managed -> ignored
      validField = true;
    
    end
    if ~validField
      msg = sprintf(_('Invalid name-value argument: %s.'), args{i});
      error(msg);
    end
  end
end
%=============================================================================
function r = formatVariable(variable, usedFormat)
  format(usedFormat);
  c = evalc('disp(variable)');
  if strcmp(usedFormat.LineSpacing, 'loose')  
    c(end) = [];
  end
  r = string(c);
end
%=============================================================================
function validateNumericFormat(str, pos)
  mustBeMember(lower(str), [...
  "short", ...
  "long", ...
  "shorte", ...
  "longe", ...
  "shortg", ...
  "longg", ...
  "shorteng", ...
  "longeng", ...
  "+", ...
  "bank", ...
  "hex", ...
  "rational"], pos);
end
%=============================================================================
function validateLineSpacing(str, pos)
  mustBeMember(lower(str),["loose","compact"], pos);
end
%=============================================================================

