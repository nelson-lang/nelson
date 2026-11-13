%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = unzip(zipfilename, varargin)
  % Extracts a ZIP archive file or URL (http, https) into a folder (current folder by default).
  % MaxExtractedSize and MaxCompressionRatio refuse archives that would expand too much.
  [folder, options] = nelson.io.internal.extractionOptions(varargin);
  if nelson.io.internal.isUrl(zipfilename)
    zipfilename = nelson.io.internal.downloadUrl(zipfilename);
    cleanup = onCleanup(@() delete(zipfilename));
  end
  [varargout{1:nargout}] = __unzip__(zipfilename, folder, options.MaxExtractedSize, options.MaxCompressionRatio);
end
%=============================================================================
