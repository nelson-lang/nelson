%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = timezones()
  timezoneDataPath = [modulepath('time'), '/resources/tzdata'];
  [zones, version] = timezoneslist(timezoneDataPath);
  varargout{1} = zones;
  if nargout > 1
    varargout{2} = version;
  end
end
%=============================================================================
