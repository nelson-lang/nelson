%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function fid = fopen(z, name, varargin)
  % File identifier on an entry: fread, fgetl, textscan, fwrite, fprintf... then fclose.
  fid = z.fopen(name, varargin{:});
end
%=============================================================================
