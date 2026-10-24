%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function pds = parquetDatastore(location, varargin)
  pds = nelson.io.datastore.ParquetDatastore(location, varargin{:});
end
%=============================================================================
