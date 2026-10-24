%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = removets(obj, name)
  name = tscollectionValidName(timeseriesText(name, 'Timeseries name'));
  if isfield(obj.Members_, name)
    obj.Members_ = rmfield(obj.Members_, name);
  end
end
%=============================================================================
