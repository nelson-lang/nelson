%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) ZipArchive < handle & nelson.mixin.CustomDisplay
  % Open ZIP archive: list, read, write or append entries without extracting files.
  properties (SetAccess = private)
    Source = ""
    Mode = "r"
    Compression = "deflate"
    Level = -1
    Encryption = "none"
  end
  properties (Dependent)
    IsOpen
  end
  properties (Access = private, Hidden)
    Id = 0
    Bytes = uint8([])
    InMemory = false
    DownloadedFile = ''
  end
  methods
    function obj = ZipArchive(source, mode, varargin)
      narginchk(1, Inf);
      obj.Mode = "r";
      if nargin >= 2
        obj.Mode = string(lower(char(mode)));
      end
      [password, encryption, options] = securityOptions(obj.Mode, varargin);
      if ~isempty(password) && obj.Mode ~= "r"
        obj.Encryption = string(lower(encryption));
      end
      [obj.Compression, obj.Level] = parseCompressionOptions("deflate", -1, options{:});
      s = nelson.io.internal.archiveSource(source, obj.Mode);
      target = s.Target;
      obj.InMemory = s.InMemory;
      obj.Bytes = s.Bytes;
      obj.Source = s.Source;
      obj.DownloadedFile = s.DownloadedFile;
      if isempty(password)
        obj.Id = __zip_open__(target, char(obj.Mode));
      else
        obj.Id = __zip_open__(target, char(obj.Mode), password, char(encryption));
      end
    end

    function tf = get.IsOpen(obj)
      tf = obj.Id ~= 0;
    end

    function T = entries(obj)
      s = __zip_entries__(obj.Id);
      Modified = datetime(s.Modified, 'ConvertFrom', 'posixtime', 'TimeZone', 'local');
      Modified.TimeZone = '';
      T = table(s.Name, s.Size, s.CompressedSize, s.Compression, Modified, s.IsFolder, s.CRC32, s.Encrypted, ...
        'VariableNames', {'Name', 'Size', 'CompressedSize', 'Compression', 'Modified', 'IsFolder', 'CRC32', ...
        'Encrypted'});
    end

    function data = read(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('MaxSize', Inf), varargin{:});
      data = __zip_read__(obj.Id, char(name), double(options.MaxSize));
    end

    function txt = readtext(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('Encoding', 'UTF-8', 'MaxSize', Inf), varargin{:});
      txt = native2unicode(obj.read(name, 'MaxSize', options.MaxSize), char(options.Encoding));
    end

    function write(obj, name, data, varargin)
      [compression, level, modified] = parseCompressionOptions(obj.Compression, obj.Level, varargin{:});
      __zip_write__(obj.Id, char(name), nelson.io.internal.bytesFromData(data), char(compression), level, modified);
    end

    function names = add(obj, path, varargin)
      entryName = '';
      options = varargin;
      if mod(numel(options), 2) == 1
        entryName = char(options{1});
        options(1) = [];
      end
      [compression, level] = parseCompressionOptions(obj.Compression, obj.Level, options{:});
      names = __zip_add__(obj.Id, char(path), entryName, char(compression), level);
    end

    function fid = fopen(obj, name, mode, varargin)
      if nargin < 3
        mode = 'r';
      end
      [encoding, options] = nelson.io.internal.fopenOptions(struct('MaxSize', Inf, 'Compression', obj.Compression, ...
        'Level', obj.Level, 'Modified', []), varargin{:});
      [compression, level, modified] = parseCompressionOptions(obj.Compression, obj.Level, ...
        'Compression', options.Compression, 'Level', options.Level, 'Modified', options.Modified);
      if obj.InMemory
        displayName = ['<bytes>:', char(name)];
      else
        displayName = [char(obj.Source), ':', char(name)];
      end
      fid = __zip_fopen__(obj.Id, char(name), displayName, char(mode), encoding, ...
        double(options.MaxSize), char(compression), level, modified);
    end

    function bytes = tobytes(obj)
      if ~obj.InMemory
        error('Nelson:io:zip:notInMemory', 'Only an in-memory archive can be returned as bytes.');
      end
      if obj.IsOpen && obj.Mode == "w"
        obj.close();
      end
      bytes = obj.Bytes;
    end

    function close(obj)
      if obj.Id == 0
        return
      end
      bytes = __zip_close__(obj.Id);
      obj.Id = 0;
      obj.removeDownload();
      if obj.InMemory && obj.Mode == "w"
        obj.Bytes = bytes;
      end
    end

    function delete(obj)
      % An entry still written through a file identifier closes the archive when it ends.
      if obj.Id ~= 0
        try
          __zip_close__(obj.Id, true);
        catch
        end
        obj.Id = 0;
      end
      obj.removeDownload();
    end
  end
  methods (Access = private)
    function removeDownload(obj)
      % The temporary copy of a URL is deleted once the archive is closed.
      if ~isempty(obj.DownloadedFile) && isfile(obj.DownloadedFile)
        try
          delete(obj.DownloadedFile);
        catch
        end
      end
      obj.DownloadedFile = '';
    end
  end
end
%=============================================================================
function [password, encryption, rest] = securityOptions(mode, options)
  % 'Password' (reading, and encryption of new files in write modes) and 'Encryption' options.
  [password, rest] = takeTextOption('Password', options);
  [encryption, rest] = takeTextOption('Encryption', rest);
  if ~isempty(encryption) && (isempty(password) || mode == "r")
    error('Nelson:io:zip:encryptionNeedsPassword', 'Encryption needs a Password and a write mode.');
  end
  if isempty(encryption)
    encryption = 'aes256';
  end
end
%=============================================================================
function [value, rest] = takeTextOption(name, options)
  % Removes the name-value pair of a text option from the other options ('' when absent).
  value = '';
  isName = cellfun(@(x) isTextScalar(x) && strcmpi(x, name), options);
  isName(2:2:end) = false;
  rest = options;
  for k = fliplr(find(isName))
    if k == numel(options) || ~isTextScalar(options{k + 1})
      error('Nelson:io:zip:invalidTextOption', '%s must be a character vector or a string scalar.', name);
    end
    value = char(options{k + 1});
    rest(k:k + 1) = [];
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = (ischar(value) && (isrow(value) || isempty(value))) || (isstring(value) && isscalar(value));
end
%=============================================================================
function [compression, level, modified] = parseCompressionOptions(compression, level, varargin)
  defaults = struct('Compression', compression, 'Level', level, 'Modified', []);
  options = nelson.io.internal.nameValueOptions(defaults, varargin{:});
  compression = string(lower(char(options.Compression)));
  level = double(options.Level);
  modified = 0;
  if ~isempty(options.Modified)
    modified = nelson.io.internal.posixSeconds(options.Modified);
  end
end
%=============================================================================
