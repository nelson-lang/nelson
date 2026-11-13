%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function z = open(source, varargin)
  % Opens a ZIP archive file, archive bytes, or [] with mode "w" for a new in-memory archive.
  z = nelson.io.ZipArchive(source, varargin{:});
end
%=============================================================================
