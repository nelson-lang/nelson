%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ClassdefSubsasgnEndProbe
  methods
    function value = subsref(obj, S)
      global CLASSDEF_SUBSASGN_END_TRACE;
      CLASSDEF_SUBSASGN_END_TRACE{end + 1} = struct('kind', 'subsref', 'S', S);
      value = 1:5;
    end
    function obj = subsasgn(obj, S, value)
      global CLASSDEF_SUBSASGN_END_TRACE;
      CLASSDEF_SUBSASGN_END_TRACE{end + 1} = struct('kind', 'subsasgn', 'S', S, 'value', value);
    end
  end
end
