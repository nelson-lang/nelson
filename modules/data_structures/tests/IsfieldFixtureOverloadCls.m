%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef IsfieldFixtureOverloadCls
  properties
    x = 1
  end
  methods
    function r = isfield(obj, name)
      r = strcmp(name, 'custom');
    end
  end
end
