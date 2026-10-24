%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nargchk is the deprecated predecessor of narginchk. It is kept for
% compatibility with legacy toolboxes that call error(nargchk(low, high, n)).
function msg = nargchk(low, high, n, outputType)
  narginchk(3, 4);
  nargoutchk(0, 1);
  if nargin < 4
    outputType = 'string';
  end
  if ~(isnumeric(low) && isscalar(low))
    error('Nelson:nargchk:mustBeNumeric', _('Wrong type for argument #%d: scalar integer value required.'), 1);
  end
  if ~(isnumeric(high) && isscalar(high))
    error('Nelson:nargchk:mustBeNumeric', _('Wrong type for argument #%d: scalar integer value required.'), 2);
  end
  if ~(isnumeric(n) && isscalar(n))
    error('Nelson:nargchk:mustBeNumeric', _('Wrong type for argument #%d: scalar integer value required.'), 3);
  end
  if ~(ischar(outputType) || (isstring(outputType) && isscalar(outputType)))
    error('Nelson:types:wrongArgumentTypeExpected', _('Wrong type for argument #%d: %s expected.'), 4, 'string');
  end
  outputType = char(outputType);
  wantStruct = strcmp(outputType, 'struct');
  if ~(wantStruct || strcmp(outputType, 'string'))
    error('Nelson:nargchk:unknownOutputType', _('Output type must be ''string'' or ''struct''.'));
  end
  message = '';
  identifier = '';
  if n < low
    message = _('Not enough input arguments.');
    identifier = 'Nelson:nargchk:notEnoughInputs';
  elseif n > high
    message = _('Too many input arguments.');
    identifier = 'Nelson:nargchk:tooManyInputs';
  end
  if wantStruct
    if isempty(message)
      % In range: return a 1x0 empty struct with the expected fields.
      msg = struct('message', {}, 'identifier', {});
    else
      msg = struct('message', message, 'identifier', identifier);
    end
  else
    msg = message;
  end
end
%=============================================================================
