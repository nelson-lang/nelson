%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytes = urlBytes(url)
  % Content of url as a uint8 row vector.
  filename = nelson.io.internal.downloadUrl(url);
  cleanup = onCleanup(@() delete(filename));
  fid = fopen(filename, 'r');
  bytes = fread(fid, Inf, 'uint8=>uint8')';
  fclose(fid);
end
%=============================================================================
