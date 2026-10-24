%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef vartype
  properties (Hidden)
    TypeName
  end
  methods
    function obj = vartype(typeName)
      narginchk(1, 1);
      obj.TypeName = char(typeName);
    end
    function idx = resolve(obj, T)
      st = struct(T);
      types = st.Properties.VariableTypes;
      idx = find(strcmp(types, obj.TypeName));
    end
  end
end
%=============================================================================
