%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filenames = gzip(files, outputFolder)
  % Compresses files (names, wildcards or folders) into .gz files.
  narginchk(1, 2);
  sources = nelson.io.internal.expandFileList(files, 'Nelson:gzip:fileNotFound');
  filenames = cell(1, numel(sources));
  for k = 1:numel(sources)
    [~, name, ext] = fileparts(sources{k});
    if nargin > 1
      if ~isfolder(outputFolder)
        mkdir(outputFolder);
      end
      destination = fullfile(char(outputFolder), [name, ext, '.gz']);
    else
      destination = [sources{k}, '.gz'];
    end
    filenames{k} = nelson.io.gzip.compressfile(sources{k}, destination);
  end
end
%=============================================================================
