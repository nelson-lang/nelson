%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isavailable()
  % True when this build can compress and decompress gzip data.
  tf = strlength(__stream_version__('gzip')) > 0;
end
%=============================================================================
