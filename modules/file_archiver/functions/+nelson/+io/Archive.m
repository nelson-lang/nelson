%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) Archive < handle & nelson.mixin.CustomDisplay
  % Read-only archive of any supported format (7z, rar, zip, tar, iso, cpio...): list, read or extract entries.
  properties (SetAccess = private)
    Source = ""
    Format = ""
    Compression = "none"
  end
  properties (Dependent)
    IsOpen
  end
  properties (Access = private, Hidden)
    Id = 0
    InMemory = false
    DownloadedFile = ''
  end
  methods
    function obj = Archive(source, varargin)
      narginchk(1, Inf);
      options = nelson.io.internal.nameValueOptions(struct('Password', ''), varargin{:});
      if ~(ischar(options.Password) || (isstring(options.Password) && isscalar(options.Password)))
        error('Nelson:io:archive:invalidPassword', 'Password must be a character vector or a string scalar.');
      end
      s = nelson.io.internal.archiveSource(source, "r");
      if isnumeric(s.Target) && isempty(s.Target)
        error('Nelson:io:archive:invalidSource', 'Source must be a file name or uint8 data.');
      end
      obj.InMemory = s.InMemory;
      obj.Source = s.Source;
      obj.DownloadedFile = s.DownloadedFile;
      obj.Id = __archive_open__(s.Target, char(options.Password));
      info = __tar_entries__(obj.Id);
      obj.Format = string(info.Format);
      obj.Compression = string(info.Compression);
    end

    function tf = get.IsOpen(obj)
      tf = obj.Id ~= 0;
    end

    function T = entries(obj)
      s = __tar_entries__(obj.Id);
      Modified = datetime(s.Modified, 'ConvertFrom', 'posixtime', 'TimeZone', 'local');
      Modified.TimeZone = '';
      T = table(s.Name, s.Size, Modified, s.Mode, s.IsFolder, s.IsLink, s.Encrypted, ...
        'VariableNames', {'Name', 'Size', 'Modified', 'Mode', 'IsFolder', 'IsLink', 'Encrypted'});
    end

    function data = read(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('MaxSize', Inf), varargin{:});
      data = __tar_read__(obj.Id, char(name), double(options.MaxSize));
    end

    function txt = readtext(obj, name, varargin)
      options = nelson.io.internal.nameValueOptions(struct('Encoding', 'UTF-8', 'MaxSize', Inf), varargin{:});
      txt = native2unicode(obj.read(name, 'MaxSize', options.MaxSize), char(options.Encoding));
    end

    function fid = fopen(obj, name, varargin)
      [encoding, options] = nelson.io.internal.fopenOptions(struct('MaxSize', Inf), varargin{:});
      if obj.InMemory
        displayName = ['<bytes>:', char(name)];
      else
        displayName = [char(obj.Source), ':', char(name)];
      end
      fid = __tar_fopen__(obj.Id, char(name), displayName, 'r', encoding, double(options.MaxSize));
    end

    function filenames = extract(obj, varargin)
      [folder, options] = nelson.io.internal.extractionOptions(varargin);
      filenames = __tar_extract__(obj.Id, folder, options.MaxExtractedSize, options.MaxCompressionRatio);
    end

    function close(obj)
      if obj.Id == 0
        return
      end
      __tar_close__(obj.Id);
      obj.Id = 0;
      obj.removeDownload();
    end

    function delete(obj)
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
