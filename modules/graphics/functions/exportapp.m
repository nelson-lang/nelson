%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function exportapp(varargin)
  % exportapp(fig, filename)
  % Save the contents of a figure (including UI components) to an image file.
  narginchk(2, 2);
  nargoutchk(0, 0);

  fig = varargin{1};
  filename = varargin{2};

  if ~(isscalar(fig) && isgraphics(fig, 'figure'))
    error('Nelson:exportapp:invalidFigure', 'First argument must be a single figure handle.');
  end

  if (isStringScalar(filename))
    filename = char(filename);
  end
  if ~(ischar(filename) && isrow(filename))
    error('Nelson:exportapp:invalidFilename', 'Second argument must be a filename as a character vector or string scalar.');
  end

  frame = getframe(fig);
  imwrite(frame.cdata, filename);
end
%=============================================================================
