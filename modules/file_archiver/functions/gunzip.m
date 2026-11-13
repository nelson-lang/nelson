%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filenames = gunzip(files, varargin)
  % Decompresses .gz files (names, wildcards, folders or one URL); the output drops the .gz extension.
  % MaxExtractedSize and MaxCompressionRatio refuse files that would expand too much.
  hasFolder = mod(numel(varargin), 2) == 1;
  [outputFolder, options] = nelson.io.internal.extractionOptions(varargin);
  limits = {'MaxSize', options.MaxExtractedSize, 'MaxCompressionRatio', options.MaxCompressionRatio};
  if nelson.io.internal.isUrl(files)
    filenames = gunzipUrl(files, outputFolder, limits);
    return
  end
  sources = nelson.io.internal.expandFileList(files, 'Nelson:gunzip:fileNotFound');
  sources = sources(endsWith(sources, '.gz', 'IgnoreCase', true));
  filenames = cell(1, numel(sources));
  for k = 1:numel(sources)
    [folder, name] = fileparts(sources{k});
    if hasFolder
      if ~isfolder(outputFolder)
        mkdir(outputFolder);
      end
      folder = outputFolder;
    end
    filenames{k} = nelson.io.gzip.decompressfile(sources{k}, fullfile(folder, name), limits{:});
  end
end
%=============================================================================
function filenames = gunzipUrl(url, folder, limits)
  % The output keeps the name of the URL path.
  [~, name] = fileparts(regexprep(char(url), '[?#].*$', ''));
  downloaded = nelson.io.internal.downloadUrl(url);
  cleanup = onCleanup(@() delete(downloaded));
  if ~isfolder(folder)
    mkdir(folder);
  end
  filenames = {nelson.io.gzip.decompressfile(downloaded, fullfile(char(folder), name), limits{:})};
end
%=============================================================================
