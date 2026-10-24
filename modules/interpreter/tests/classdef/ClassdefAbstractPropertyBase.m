%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Abstract) ClassdefAbstractPropertyBase
  properties (Abstract)
    RequiredValue
  end

  properties (Abstract, Access = protected)
    ProtectedValue(1, 1) double
  end
end
