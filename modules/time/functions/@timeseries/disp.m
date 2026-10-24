%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function disp(obj)
  disp(sprintf('  timeseries: %d samples', obj.Length));
  if ~isempty(obj.Name)
    disp(['  Name: ', obj.Name]);
  end
end
%=============================================================================
