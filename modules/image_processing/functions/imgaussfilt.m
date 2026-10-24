%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imgaussfilt(varargin)
  narginchk(1, 8);
  nargoutchk(0, 1);
  argIndex = 2;
  if nargin >= 2 && ~(ischar(varargin{2}) || isstring(varargin{2}))
    argIndex = 3;
  end
  for k = argIndex:2:nargin
    if k == nargin
      error('Nelson:image_processing:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
    end
    if ~(ischar(varargin{k}) || isstring(varargin{k}))
      error('Nelson:image_processing:imgaussfiltOptionNameString', '%s option name must be a string.', 'imgaussfilt');
    end
  end
  out = builtin('imgaussfilt', varargin{:});
end
%=============================================================================
