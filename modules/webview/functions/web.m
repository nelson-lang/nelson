%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = web(varargin)
  if nargout > 3
    error('Nelson:web:wrongNumberOutputArguments2', 'Wrong number of output arguments.');
  end

  [location, hasLocation, options] = parseInputs(varargin{:});
  if ~hasLocation
    location = '';
  end

  status = 0;
  viewer = [];
  currentUrl = '';

  if options.UseSystemBrowser
    status = openSystemBrowser(location);
  elseif ~strcmp(getnelsonmode(), 'GUI')
    viewer = nelson.htmlviewer.htmlviewer.createFallback(location);
    currentUrl = viewer.Input;
  else
    try
      viewer = nelson.htmlviewer.htmlviewer.create(location, options.NewWindow, ...
        options.ShowToolbar, options.ShowAddressBox);
      currentUrl = viewer.Input;
    catch ex
      if nargout == 0
        viewer = nelson.htmlviewer.htmlviewer.createFallback(location);
        return
      end
      viewer = nelson.htmlviewer.htmlviewer.createFallback(location);
      currentUrl = viewer.Input;
    end
  end

  if nargout > 0
    varargout{1} = status;
  end
  if nargout > 1
    varargout{2} = viewer;
  end
  if nargout > 2
    varargout{3} = currentUrl;
  end
end
%=============================================================================
function [location, hasLocation, options] = parseInputs(varargin)
  location = '';
  hasLocation = false;
  options = defaultOptions();

  for k = 1:nargin
    value = validateTextScalar(varargin{k}, k);
    value = strtrim(value);
    if isempty(value)
      continue
    end
    if strncmp(value, '-', 1)
      options = applyOption(options, value);
    elseif ~hasLocation
      location = value;
      hasLocation = true;
    end
  end

  if hasLocation && isSystemBrowserLocation(location)
    options.UseSystemBrowser = true;
  end
end
%=============================================================================
function options = defaultOptions()
  options = struct();
  options.UseSystemBrowser = false;
  options.NewWindow = false;
  options.ShowToolbar = true;
  options.ShowAddressBox = true;
end
%=============================================================================
function options = applyOption(options, value)
  switch lower(value)
    case '-browser'
      options.UseSystemBrowser = true;
    case '-new'
      options.NewWindow = true;
    case '-noaddressbox'
      options.ShowAddressBox = false;
    case '-notoolbar'
      options.ShowToolbar = false;
      options.ShowAddressBox = false;
    otherwise
      error('Nelson:web:unsupportedOption', 'Unsupported web option.');
  end
end
%=============================================================================
function tf = isSystemBrowserLocation(location)
  value = lower(location);
  tf = startsWith(value, 'http://') || startsWith(value, 'https://') ...
    || startsWith(value, 'mailto:');
end
%=============================================================================
function status = openSystemBrowser(location)
  status = 0;
  try
    if ispc()
      winopen(location);
    elseif ismac()
      [cmdStatus, ~] = unix(['open ', shellQuote(location), ' &']);
      status = statusFromCommand(cmdStatus);
    else
      [cmdStatus, ~] = unix(['xdg-open ', shellQuote(location), ' &']);
      status = statusFromCommand(cmdStatus);
    end
  catch
    lasterror('reset');
    status = 2;
  end
end
%=============================================================================
function status = statusFromCommand(cmdStatus)
  if cmdStatus == 0
    status = 0;
  else
    status = 2;
  end
end
%=============================================================================
function value = shellQuote(value)
  value = ['"', strrep(value, '"', '\"'), '"'];
end
%=============================================================================
function value = validateTextScalar(value, index)
  supported = (ischar(value) && size(value, 1) == 1) || (isstring(value) && isscalar(value));
  if ~supported
    error('Nelson:invalidType', 'Wrong type for argument #%d: %s expected.', index, 'char or string');
  end
  value = convertStringsToChars(value);
end
%=============================================================================
