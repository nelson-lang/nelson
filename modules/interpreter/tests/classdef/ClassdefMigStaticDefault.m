%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefMigStaticDefault
  properties
    s = ClassdefMigStaticDefault.stat()
  end
  methods (Static)
    function r = stat()
      r = 99;
    end
  end
end
