%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = urlencode(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  inputString = varargin{1};
  mustBeTextScalar(inputString, 1);
  inputString = convertStringsToChars(inputString);
  encodedString = '';
  for i = 1:length(inputString)
    currentChar = inputString(i);
    if strcmp(currentChar, ' ')
      encodedString = [encodedString, '+'];
    elseif ismember(currentChar, ['a':'z', 'A':'Z', '0':'9', '-', '_', '.', '*'])
      encodedString = [encodedString, currentChar];
    else
      decimalChars = unicode2native(currentChar, 'UTF-8');
      hexaChars = dec2hex(decimalChars, 2);
      for j = 1:size(hexaChars, 1)
        encodedString = [encodedString, '%', hexaChars(j,:)];
      end
    end
  end
  varargout{1} = encodedString;
end
%=============================================================================
