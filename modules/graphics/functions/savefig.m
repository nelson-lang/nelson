%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function savefig(varargin)
  narginchk(0, 3);
  nargoutchk(0, 0);

  version = '';
  if nargin == 0
    fig = gcf();
    filename = 'Untitled.fig';
  elseif nargin == 1
    if isgraphics(varargin{1}, 'figure')
      fig = varargin{1};
      filename = 'Untitled.fig';
    else
      fig = gcf();
      filename = localScalarText(varargin{1});
    end
  else
    fig = varargin{1};
    filename = localScalarText(varargin{2});
    if nargin == 3
      version = lower(localScalarText(varargin{3}));
    end
  end

  if ~isgraphics(fig, 'figure')
    error('Nelson:savefig:expectedFigure', 'Expected figure graphics object(s).');
  end
  if ~isempty(version) && ~any(strcmp(version, {'-v7.3', '-v7', '-v6'}))
    error('Nelson:savefig:validVersionExpected', 'Valid version expected.');
  end

  filename = localFigFilename(filename, true);
  nelson_fig_payload = __savefig__(fig);
  savenh5(filename, 'nelson_fig_payload');
end
%=============================================================================
function txt = localScalarText(value)
  if isStringScalar(value)
    txt = char(value);
  elseif ischar(value)
    txt = value;
  else
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Filename');
  end
  if isempty(txt)
    error('Nelson:graphics:filenameIsEmpty', 'Filename is empty.');
  end
end
%=============================================================================
function filename = localFigFilename(filename, forSave)
  if startsWith(filename, '-')
    error('Nelson:savefig:invalidFilename', 'Invalid filename.');
  end
  [pathstr, name, ext] = fileparts(filename);
  if isempty(ext)
    filename = [filename, '.fig'];
  elseif ~strcmpi(ext, '.fig')
    error('Nelson:graphics:figFileExpected', 'FIG file expected.');
  end
  if forSave && ~isempty(pathstr) && ~isdir(pathstr)
    error('Nelson:savefig:directoryDoesNotExist', 'Parent directory does not exist: %s', pathstr);
  end
  filename = fullpath(filename);
end
%=============================================================================
