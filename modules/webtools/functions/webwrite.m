%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = webwrite(varargin)
  if nargin < 2
    error(_('Wrong number of input arguments.'));
  end
  if nargout > 1
    error(_('Wrong number of output arguments.'));
  end
  options = [];
  url = getArgumentAsCharactersVector(varargin{1});
  tmpfile = buildTempFilename(url);
  st = struct();
  switch nargin
    case 2
      if ischar(varargin{2}) || (isstring(varargin{2}) && isscalar(varargin{2}))
        data = varargin{2};
      else
        data = jsonencode(varargin{2});
      end
      options = weboptions();
    case 3
      if ~isWebOptions(varargin{3})
        error(_('Invalid #3 Argument: weboptions expected.'));
      end
      options = varargin{3};
      if ischar(varargin{2}) || (isstring(varargin{2}) && isscalar(varargin{2}))
        data = varargin{2};
      else
        data = jsonencode(varargin{2});
      end
    otherwise
      options_position = -1;
      for k = 2:nargin
        p = varargin{k};
        if isWebOptions(p)
          options = p;
          options_position = k;
          break;
        end
      end
      if isempty(options)
        options = weboptions();
        options_position = -1;
      end
      if ~(options_position == -1 || options_position == nargin)
        error(_('weboptions must be last argument.'));
      end
      if options_position == -1
        last = nargin;
      else
        last = nargin - 1;
      end
      for k = [2:2:last]
        st.(varargin{k}) = varargin{k + 1};
      end
      data = '__WEBWRITE__';
    end
    if strcmp(options.RequestMethod, 'auto')
      options.RequestMethod = 'post';
    end
    if strcmp(options.MediaType, 'auto') == 1
      options.MediaType = 'application/x-www-form-urlencoded';
    end
    tmpfile = webREST(url, data, tmpfile, st, options);
    res = convertContentType(tmpfile, options.ContentType, options.CharacterEncoding);
    if isfile(tmpfile)
      rmfile(tmpfile)
    end
    nbElements = length(res);
    switch nbElements
      case 0
        varargout{1} = res{1};
      case 1
        varargout{1} = res{1};
      case 2
        varargout{1} = res{1};
        varargout{2} = res{2};
      otherwise
        error(_('unmanaged case.'));
      end
    end
    %=============================================================================
function res = convertContentType(filename, contentType, characterEncoding)
  res = {};
  switch contentType
    case 'auto'
      res = autoConvert(filename, characterEncoding);
    case 'text'
      txt = fileread(filename, 'char', 'native', characterEncoding);
      res = {txt};
    case 'audio'
      [y, fs] = audioread(filename);
      res = {y, fs};
    case 'binary'
      fid = fopen(filename, 'r');
      data = fread(fid, 'uint8');
      res = {data};
      fclose(fid);
    case 'json'
      txt = fileread(filename, 'char', 'native', characterEncoding);
      json = jsondecode(txt);
      res = {json};
    case 'raw'
      fid = fopen(filename, 'r');
      data = fread(fid, 'uint8');
      res = {data};
      fclose(fid);
    otherwise
      fid = fopen(filename, 'r');
      data = fread(fid, 'uint8');
      res = {data};
      fclose(fid);
    end
  end
  %=============================================================================
function res = autoConvert(filename, characterEncoding)
  res = {};
  if isempty(res)
    try
      [y, fs] = audioread(filename);
      res = {y, fs};
    catch
      res = {};
    end
  end
  if isempty(res)
    try
      txt = fileread(filename, 'char', 'native', characterEncoding);
      json = jsondecode(txt);
      res = {json};
    catch
      res = {};
    end
  end
  if isempty(res)
    try
      txt = fileread(filename, 'char', 'native', characterEncoding);
      res = {txt};
    catch
      res = {};
    end
  end
  if isempty(res)
    try
      fid = fopen(filename, 'r');
      data = fread(fid, 'uint8');
      res = {data};
      fclose(fid);
    catch
      res = {};
    end
  end
end
%=============================================================================
function str = getArgumentAsCharactersVector(arg)
  str = arg;
  supported = (isstring(str) && isscalar(str)) || (ischar(str) && (size(str, 1) == 1));
  if (~supported)
    error(_('Invalid type for #1 input argument: ''char'' or ''string'' expected.'));
  end
  str = convertStringsToChars(str);
end
%=============================================================================
function tempfilename = buildTempFilename(url)
  [p, f, e] = fileparts(url);
  if (contains(url, '?') || isempty(e))
    tempfilename = tempname();
  else
    tempfilename = [tempname(), e];
  end
end
%=============================================================================
function r = isWebOptions(arg)
  r = strcmp(class(arg), 'weboptions');
end
%=============================================================================
