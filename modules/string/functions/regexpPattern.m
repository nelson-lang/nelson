%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pat = regexpPattern(expression, varargin)
  narginchk(1, Inf);
  nargoutchk(0, 1);
  if isstring(expression)
    expression = char(expression);
  end
  k = 1;
  while k <= numel(varargin)
    name = char(varargin{k});
    switch lower(name)
      case 'ignorecase'
        value = logical(varargin{k + 1});
        if value
          expression = ['(?i:', expression, ')'];
        end
      case 'freespacing'
        value = logical(varargin{k + 1});
        if value
          expression = ['(?x:', expression, ')'];
        end
      case 'dotexceptnewline'
        value = logical(varargin{k + 1});
        if value
          expression = ['(?-s:', expression, ')'];
        end
      case 'anchors'
        mode = lower(char(varargin{k + 1}));
        if strcmp(mode, 'line')
          expression = ['(?m:', expression, ')'];
        end
      otherwise
        error('Nelson:string:unsupportedOption', 'Unsupported option.');
    end
    k = k + 2;
  end
  pat = pattern(expression);
end
%=============================================================================
