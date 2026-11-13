%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = untar(tarfilename, varargin)
  % Extracts a tar archive file or URL (plain, gzip or Zstandard) into a folder (current folder by default).
  % MaxExtractedSize and MaxCompressionRatio refuse archives that would expand too much.
  [folder, options] = nelson.io.internal.extractionOptions(varargin);
  if nelson.io.internal.isUrl(tarfilename)
    tarfilename = nelson.io.internal.downloadUrl(tarfilename);
    cleanup = onCleanup(@() delete(tarfilename));
  end
  [varargout{1:nargout}] = __untar__(tarfilename, folder, options.MaxExtractedSize, options.MaxCompressionRatio);
end
%=============================================================================
