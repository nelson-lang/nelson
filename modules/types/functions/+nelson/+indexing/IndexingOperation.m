%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef IndexingOperation
  % IndexingOperation  One element of a chained indexing expression.
  %   Type    : nelson.indexing.IndexingOperationType
  %   Indices : cell array of the paren/brace indices (empty for Dot)
  %   Name    : the field name (only for Dot)
  properties
    Type
    Indices = {}
    Name = ''
  end
  methods
    function obj = IndexingOperation(type, indices, name)
      if nargin >= 1
        if ischar(type) || isstring(type)
          obj.Type = nelson.indexing.IndexingOperation.typeFromName(char(type));
        else
          obj.Type = type;
        end
      end
      if nargin >= 2
        obj.Indices = indices;
      end
      if nargin >= 3
        obj.Name = name;
      end
    end
  end
  methods (Static, Access = private)
    function t = typeFromName(name)
      switch name
        case 'Paren'
          t = nelson.indexing.IndexingOperationType.Paren;
        case 'Brace'
          t = nelson.indexing.IndexingOperationType.Brace;
        case 'Dot'
          t = nelson.indexing.IndexingOperationType.Dot;
        case 'ParenDelete'
          t = nelson.indexing.IndexingOperationType.ParenDelete;
        case 'BraceDelete'
          t = nelson.indexing.IndexingOperationType.BraceDelete;
        otherwise
          error('Nelson:indexing:invalidType', 'Unknown indexing operation type.');
      end
    end
  end
end
%=============================================================================
