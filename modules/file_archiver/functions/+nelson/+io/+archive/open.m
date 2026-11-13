%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function a = open(source, varargin)
  % Opens an archive of any supported format (file, URL or uint8 bytes) for reading.
  a = nelson.io.Archive(source, varargin{:});
end
%=============================================================================
