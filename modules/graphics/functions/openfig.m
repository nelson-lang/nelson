%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = openfig(varargin)
  narginchk(0, 3);
  nargoutchk(0, 1);

  filename = 'Untitled.fig';
  reuse = false;
  visibleAction = '';
  firstOption = 1;
  if nargin >= 1
    filename = localScalarText(varargin{1});
    firstOption = 2;
  end

  for k = firstOption:nargin
    [reuse, visibleAction] = localCheckOption(varargin{k}, reuse, visibleAction);
  end

  filename = localFigFilename(filename);
  loaded = loadnh5(filename, 'nelson_fig_payload');
  if ~isfield(loaded, 'nelson_fig_payload')
    error('Nelson:openfig:invalidFile', 'Invalid Nelson figure file.');
  end
  fig = __openfig__(loaded.nelson_fig_payload, filename, visibleAction, reuse);
  if nargout == 1
    varargout{1} = fig;
  else
    varargout = {};
  end
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
function filename = localFigFilename(filename)
  [~, ~, ext] = fileparts(filename);
  if isempty(ext)
    filename = [filename, '.fig'];
  elseif ~strcmpi(ext, '.fig')
    error('Nelson:graphics:figFileExpected', 'FIG file expected.');
  end
  filename = fullpath(filename);
end
%=============================================================================
function [reuse, visibleAction] = localCheckOption(value, reuse, visibleAction)
  option = lower(localScalarText(value));
  switch option
    case 'new'
      reuse = false;
    case 'reuse'
      reuse = true;
    case 'visible'
      visibleAction = 'visible';
    case 'invisible'
      visibleAction = 'invisible';
    case 'auto'
      visibleAction = '';
    otherwise
      error('Nelson:openfig:invalidOption', 'Invalid option.');
  end
end
%=============================================================================
