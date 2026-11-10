%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef (Sealed) FileProperties < handle
  % Properties of a matfile or nh5file object: source file and write access.
  properties (SetAccess = private)
    Source = '';
  end

  properties
    Writable = false;
  end

  methods
    function set.Writable(obj, isWritable)
      if ~islogical(isWritable) || ~isscalar(isWritable)
        error('Nelson:io:writableNotLogical', 'Writable must be true or false.');
      end
      obj.Writable = isWritable;
    end
  end

  methods (Hidden)
    function obj = FileProperties(source)
      obj.Source = source;
      % existing files open read-only, new files are writable
      obj.Writable = ~isfile(source);
    end
  end
end
%=============================================================================
