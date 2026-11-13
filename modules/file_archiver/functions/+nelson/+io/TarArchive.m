%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) TarArchive < handle & nelson.mixin.CustomDisplay
  % Open tar archive (plain, gzip or Zstandard): list, read or write entries without extracting files.
  properties (SetAccess = private)
    Source = ""
    Mode = "r"
    Compression = "none"
    Level = -1
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
    function obj = TarArchive(source, mode, varargin)
      narginchk(1, Inf);
      obj.Mode = "r";
      if nargin >= 2
        obj.Mode = string(lower(char(mode)));
      end
      options = nelson.io.internal.nameValueOptions(struct('Compression', 'auto', 'Level', -1), varargin{:});
      obj.Level = double(options.Level);
      s = nelson.io.internal.archiveSource(source, obj.Mode);
      target = s.Target;
      obj.InMemory = s.InMemory;
      obj.Bytes = s.Bytes;
      obj.Source = s.Source;
      obj.DownloadedFile = s.DownloadedFile;
      obj.Id = __tar_open__(target, char(obj.Mode), char(lower(string(options.Compression))), obj.Level);
      s = __tar_entries__(obj.Id);
      obj.Compression = string(s.Compression);
    end

    function tf = get.IsOpen(obj)
      tf = obj.Id ~= 0;
    end

    function T = entries(obj)
      s = __tar_entries__(obj.Id);
      Modified = datetime(s.Modified, 'ConvertFrom', 'posixtime', 'TimeZone', 'local');
      Modified.TimeZone = '';
      T = table(s.Name, s.Size, Modified, s.Mode, s.IsFolder, s.IsLink, ...
        'VariableNames', {'Name', 'Size', 'Modified', 'Mode', 'IsFolder', 'IsLink'});
    end

    function data = read(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('MaxSize', Inf), varargin{:});
      data = __tar_read__(obj.Id, char(name), double(options.MaxSize));
    end

    function txt = readtext(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('Encoding', 'UTF-8', 'MaxSize', Inf), varargin{:});
      txt = native2unicode(obj.read(name, 'MaxSize', options.MaxSize), char(options.Encoding));
    end

    function write(obj, name, data, varargin)
      options = nelson.io.internal.nameValueOptions(struct('Modified', []), varargin{:});
      modified = 0;
      if ~isempty(options.Modified)
        modified = nelson.io.internal.posixSeconds(options.Modified);
      end
      __tar_write__(obj.Id, char(name), nelson.io.internal.bytesFromData(data), modified);
    end

    function names = add(obj, path, entryName)
      if nargin < 3
        entryName = '';
      end
      names = __tar_add__(obj.Id, char(path), char(entryName));
    end

    function fid = fopen(obj, name, mode, varargin)
      if nargin < 3
        mode = 'r';
      end
      [encoding, options] = nelson.io.internal.fopenOptions(struct('MaxSize', Inf), varargin{:});
      if obj.InMemory
        displayName = ['<bytes>:', char(name)];
      else
        displayName = [char(obj.Source), ':', char(name)];
      end
      fid = __tar_fopen__(obj.Id, char(name), displayName, char(mode), encoding, double(options.MaxSize));
    end

    function filenames = extract(obj, varargin)
      [folder, options] = nelson.io.internal.extractionOptions(varargin);
      filenames = __tar_extract__(obj.Id, folder, options.MaxExtractedSize, options.MaxCompressionRatio);
    end

    function bytes = tobytes(obj)
      if ~obj.InMemory
        error('Nelson:io:tar:notInMemory', 'Only an in-memory archive can be returned as bytes.');
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
      bytes = __tar_close__(obj.Id);
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
          __tar_close__(obj.Id, true);
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
