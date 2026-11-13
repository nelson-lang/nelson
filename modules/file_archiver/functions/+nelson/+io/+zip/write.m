%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function write(z, name, data, varargin)
  % Writes uint8 data or text (UTF-8) as a new entry.
  z.write(name, data, varargin{:});
end
%=============================================================================
