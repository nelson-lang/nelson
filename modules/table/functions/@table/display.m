%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function display(varargin)
  T = varargin{1};
  if nargin == 2
    name = varargin{2};
  else
    name = inputname(1);
  end
  currentFormat = format();
  if ~isempty(name)
    if strcmp(currentFormat.LineSpacing, 'loose')
      disp(' ');
    end
    disp([name, ' ='])
  end
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  h = height(T);
  w = width(T);
  header = sprintf('  %d×%d %s', h, w, 'table');
  disp(header);
  if strcmp(currentFormat.LineSpacing, 'loose')
    disp(' ');
  end
  disp(T);
end
%=============================================================================
