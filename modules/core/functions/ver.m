%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ver(name)
  narginchk(0, 1);
  nargoutchk(0, 1);
  entry = nelsonVersionEntry();
  if nargin == 0
    result = entry;
  else
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error('Nelson:types:wrongArgumentTypeExpected', 'Wrong type for argument #%d: %s expected.', 1, 'string');
    end
    if strcmpi(char(name), entry.Name)
      result = entry;
    else
      result = repmat(entry, 0, 1);
    end
  end
  if nargout == 0
    displayVersion(result);
  else
    varargout{1} = result;
  end
end
%=============================================================================
function entry = nelsonVersionEntry()
  entry = struct('Name', 'Nelson', ...
                 'Version', strtok(version(), ' '), ...
                 'Release', ['(', version('-release'), ')'], ...
                 'Date', version('-date'));
end
%=============================================================================
function displayVersion(result)
  line = repmat('-', 1, 52);
  disp(line);
  for k = 1:numel(result)
    fprintf('%s Version: %s %s\n', result(k).Name, result(k).Version, result(k).Release);
  end
  disp(line);
end
%=============================================================================
