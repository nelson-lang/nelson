%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefHeteroA < ClassdefHeteroBase
  properties
    PA = 1
  end
  methods
    function obj = ClassdefHeteroA()
      obj.Tag = 'A';
    end
  end
end
