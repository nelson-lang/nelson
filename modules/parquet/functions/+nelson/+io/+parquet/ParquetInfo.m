%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef ParquetInfo
  properties
    Filename
    FileSize
    NumRows
    NumVariables
    NumRowGroups
    RowGroups
    RowGroupHeights
    Variables
    CreatedBy
    Version
  end
  properties (Dependent)
    VariableNames
    VariableTypes
    VariableCompression
    VariableEncoding
  end
  methods
    function obj = ParquetInfo(raw)
      if nargin == 0
        return
      end
      obj.Filename = raw.Filename;
      obj.FileSize = raw.FileSize;
      obj.NumRows = raw.NumRows;
      obj.NumVariables = raw.NumVariables;
      obj.NumRowGroups = raw.NumRowGroups;
      obj.RowGroups = raw.RowGroups;
      obj.RowGroupHeights = raw.RowGroupHeights;
      obj.Variables = raw.Variables;
      obj.CreatedBy = raw.CreatedBy;
      obj.Version = raw.Version;
    end
    function names = get.VariableNames(obj)
      names = cellstr(obj.Variables.Name);
    end
    function types = get.VariableTypes(obj)
      types = cellstr(obj.Variables.Type);
    end
    function compression = get.VariableCompression(obj)
      compression = cellstr(obj.Variables.Compression);
    end
    function encoding = get.VariableEncoding(obj)
      encoding = cellstr(obj.Variables.Encoding);
    end
  end
end
%=============================================================================
