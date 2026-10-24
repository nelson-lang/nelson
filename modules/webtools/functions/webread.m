%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = webread(varargin)
  if nargin < 1
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargout > 2
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
  url = getArgumentAsCharactersVector(varargin{1});
  options = [];
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
    error('Nelson:webtools:weboptionsMustBeLast', 'weboptions must be last argument.');
  end
  if options_position == -1
    last = nargin;
  else
    last = nargin - 1;
  end
  st = struct();
  for k= 2:2:last
    st.(varargin{k}) = varargin{k + 1};
  end
  tmpfile = buildTempFilename(url);
  if strcmp(options.MediaType, 'auto') == 1
    options.MediaType = 'application/x-www-form-urlencoded';
  end
  [tmpfile, responseContentType] = webREST(url, [], tmpfile, st, options);
  if isa(options.ContentReader, 'function_handle')
    reader = options.ContentReader;
    switch nargout
      case 0
        res1 = reader(tmpfile);
        varargout = {res1};
      case 1
        res1 = reader(tmpfile);
        varargout{1} = res1;
      case 2
        [res1, res2] = reader(tmpfile);
        varargout{1} = res1;
        varargout{2} = res2;
      end
    else
      contentType = options.ContentType;
      if strcmp(contentType, 'auto')
        contentType = contentTypeFromMediaType(responseContentType);
      end
      res = convertContentType(tmpfile, contentType, options.CharacterEncoding);
      switch nargout
        case 0
          if length(res) > 0
            varargout{1} = res{1};
          else
            varargout = {};
          end
        case 1
          varargout{1} = res{1};
        case 2
          varargout{1} = res{1};
          varargout{2} = res{2};
        end
      end
      if isfile(tmpfile)
        rmfile(tmpfile)
      end
    end
    %=============================================================================
function contentType = contentTypeFromMediaType(responseContentType)
  % Maps the Content-Type header of the response to a ContentType value.
  % Unknown or missing media types, or a decoder absent from this mode (imread
  % needs a graphical mode), keep 'auto', the content sniffing path.
  contentType = 'auto';
  mediaType = lower(strtrim(strtok(responseContentType, ';')));
  if startsWith(mediaType, 'image/') && exist('imread') ~= 0
    contentType = 'image';
  elseif startsWith(mediaType, 'audio/') && exist('audioread') ~= 0
    contentType = 'audio';
  elseif endsWith(mediaType, '/json') || endsWith(mediaType, '+json')
    contentType = 'json';
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
    case 'image'
      [img, map] = imread(filename);
      if isempty(map)
        res = {img};
      else
        res = {img, map};
      end
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
    case 'table'
      res = {readtable(filename)};
    case 'xmldom'
      txt = fileread(filename, 'char', 'native', characterEncoding);
      res = {txt};
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
  try
    [y, fs] = audioread(filename);
    res = {y, fs};
    return
  catch
    lasterror('reset');
    res = {};
  end

  try
    txt = fileread(filename, 'char', 'native', characterEncoding);
    json = jsondecode(txt);
    if isstruct(json)
      res = {json};
      return
    else
      res = {};
    end
  catch
    lasterror('reset');
    res = {};
  end

  try
    txt = fileread(filename, 'char', 'native', characterEncoding);
    res = {txt};
    return
  catch
    lasterror('reset');
    res = {};
  end

  try
    fid = fopen(filename, 'r');
    data = fread(fid, 'uint8');
    res = {data};
    fclose(fid);
    return
  catch
    lasterror('reset');
    res = {};
  end
end
%=============================================================================
function str = getArgumentAsCharactersVector(arg)
  str = arg;
  supported = (isstring(str) && isscalar(str)) || (ischar(str) && (size(str, 1) == 1));
  if (~supported)
    error('Nelson:types:wrongArgumentTypeExpected', 'Wrong type for argument #%d: %s expected.', 1, '''char'' or ''string''');
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
